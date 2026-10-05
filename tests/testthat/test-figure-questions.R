# Figure questions: an image line between a stem and its options, plus opt-in
# inline math. The figure must never disturb option shuffling or the answer key.

fixture_course <- function(math = FALSE) {
  dir <- tempfile("bq-fig-"); dir.create(file.path(dir, "figures"), recursive = TRUE)
  grDevices::png(file.path(dir, "figures", "fig1.png"), width = 600, height = 400)
  graphics::plot(1:10, (1:10)^2, main = "fixture"); grDevices::dev.off()
  stem1 <- if (math) "Fig. 1 shows $R^2$ = 0.5 and costs $5 or $10. What follows?" else
    "What does Fig. 1 show?"
  writeLines(c(
    sprintf("**Question 1 [1 mark]:** %s", stem1), "",
    "![Fig. 1 from Smith et al. (2021). 50% of data_points.](figures/fig1.png){width=0.6}", "",
    "a. alpha", "b. bravo", "c. charlie", "d. delta", "e. echo", "",
    "<!-- Answer C -- because -->", "",
    "**Question 2 [1 mark]:** Plain question?", "",
    "a. one", "b. two", "c. three", "d. four", "e. five", "",
    "<!-- Answer B -- because -->", ""
  ), file.path(dir, "questions.md"))
  dir
}

test_that("tex_escape_math keeps prices literal and passes real math through", {
  expect_equal(tex_escape_math("$R^2$ is 50% high"), "$R^2$ is 50\\% high")
  expect_equal(tex_escape_math("costs $5 and $10"), "costs \\$5 and \\$10")
  expect_equal(tex_escape_math("one $ only"), "one \\$ only")
  expect_equal(tex_escape_math("a \\$5 fee"), "a \\$5 fee")
  expect_equal(tex_escape_math("$p<0.05$ & $\\beta$"), "$p<0.05$ \\& $\\beta$")
  expect_equal(tex_escape_math("$50% off$"), "$50\\% off$")
  expect_equal(tex_escape_math("snake_case & 100%"), "snake\\_case \\& 100\\%")
})

test_that("tex_escape_math is identical to tex_escape for text without dollars", {
  x <- c("a_b & c%", "~^#{}", "back\\slash", "plain")
  expect_equal(tex_escape_math(x), tex_escape(x))
})

test_that("a figure line is parsed out of the stem", {
  dir <- fixture_course()
  cfg <- test_config(1:2)
  blocks <- parse_question_blocks(file.path(dir, "questions.md"), cfg)
  expect_equal(blocks[[1]]$question, "What does Fig. 1 show?")
  expect_equal(blocks[[1]]$figure$width, 0.6)
  expect_match(blocks[[1]]$figure$caption, "Smith et al")
  expect_true(file.exists(blocks[[1]]$figure$path))
  expect_null(blocks[[2]]$figure)
})

test_that("a missing figure file stops with a clear message", {
  dir <- fixture_course(); unlink(file.path(dir, "figures", "fig1.png"))
  expect_error(parse_question_blocks(file.path(dir, "questions.md"), test_config(1:2)),
               "figure not found")
})

test_that("shuffling keeps the figure in place and the answer key correct", {
  dir <- fixture_course(); out <- file.path(dir, "output")
  cfg <- test_config(1:2)
  suppressMessages(key <- generate_versions(cfg, file.path(dir, "questions.md"), out))

  # Answer key for the figure question is the same as without the figure line.
  plain <- tempfile(fileext = ".md")
  src <- readLines(file.path(dir, "questions.md"))
  writeLines(src[!grepl("^!\\[", src)], plain)
  suppressMessages(key0 <- generate_versions(cfg, plain, tempfile()))
  expect_equal(key, key0)

  v2 <- readLines(file.path(out, "questions_v2.md"))
  i <- grep("^\\*\\*Question 1", v2)
  expect_match(v2[i + 2], "^!\\[Fig. 1")
  expect_match(v2[i + 2], "\\(\\.\\./figures/fig1\\.png\\)")   # re-pointed at outdir
  expect_true(file.exists(file.path(out, sub(".*\\]\\((.*)\\).*", "\\1", v2[i + 2]))))

  blocks <- parse_question_blocks(file.path(out, "questions_v2.md"), cfg)
  expect_false(is.null(blocks[[1]]$figure))
  expect_equal(blocks[[1]]$question, "What does Fig. 1 show?")
})

test_that("figure and math render into both paper layouts", {
  dir <- fixture_course(math = TRUE); out <- file.path(dir, "output")
  cfg <- test_config(1:2)
  suppressMessages(generate_versions(cfg, file.path(dir, "questions.md"), out))
  blocks <- parse_question_blocks(file.path(out, "questions_v1.md"), cfg)

  for (tex in list(latex_question_paper(cfg, blocks, "1"), latex_inline_quiz(cfg, blocks, "1"))) {
    tex <- paste(tex, collapse = "\n")
    expect_match(tex, "\\usepackage{graphicx}", fixed = TRUE)
    expect_match(tex, "\\includegraphics[width=0.60\\linewidth, height=7cm", fixed = TRUE)
    expect_match(tex, "$R^2$", fixed = TRUE)
    expect_match(tex, "costs \\$5 or \\$10", fixed = TRUE)
    expect_match(tex, "50\\% of data\\_points", fixed = TRUE)
  }
})

test_that("figure-free papers do not load graphicx", {
  cfg <- test_config(1:2)
  blocks <- list(list(number = 1L, question = "q", figure = NULL,
                      options = c(a = "1", b = "2", c = "3", d = "4", e = "5")))
  expect_false(any(grepl("graphicx", latex_question_paper(cfg, blocks, "1"))))
})

test_that("figure questions compile with xelatex in both layouts", {
  skip_if(Sys.which("xelatex") == "", "xelatex not available")
  skip_if_not_installed("pdftools")
  dir <- fixture_course(math = TRUE); out <- file.path(dir, "output")
  cfg <- test_config(1:2)
  suppressMessages({
    generate_versions(cfg, file.path(dir, "questions.md"), out)
    pdfs <- c(render_papers(cfg, out), make_quiz_forms(cfg, out))
  })
  pdfs <- pdfs[grepl("\\.pdf$", pdfs)]
  expect_gt(length(pdfs), 0)
  for (p in pdfs) expect_true(file.exists(p))
  text <- pdftools::pdf_text(grep("quizform_v1", pdfs, value = TRUE))
  expect_match(paste(text, collapse = " "), "Smith et al")
})
