# Code in questions: inline `code` spans and fenced code blocks between a stem
# and its options. Neither may disturb option shuffling or the answer key.

code_fixture_course <- function() {
  dir <- tempfile("bq-code-"); dir.create(dir)
  writeLines(c(
    "**Question 1 [1 mark]:** Consider this code:", "",
    "```r",
    "flow |>",
    "  group_by(month) |>",
    "",
    "  summarise(q = mean(flow_mm))   # 100% \"mean\" & $x$",
    "a. not an option",
    "<!-- Answer A -- not an answer either -->",
    "```", "",
    "What does `summarise()` return?", "",
    "a. one row per `month`", "b. bravo", "c. charlie", "d. delta", "e. echo", "",
    "<!-- Answer A -- because -->", "",
    "**Question 2 [1 mark]:** Is `read_csv(\"data/x_y.csv\")` costing $5?", "",
    "a. one", "b. two", "c. three", "d. four", "e. five", "",
    "<!-- Answer B -- because -->", ""
  ), file.path(dir, "questions.md"))
  dir
}

test_that("inline code is escaped literally and set with \\bqcode", {
  expect_equal(tex_escape_math("use `group_by()` here"), "use \\bqcode{group\\_by()} here")
  expect_equal(tex_escape_math("`x$y` and $R^2$"), "\\bqcode{x\\$y} and $R^2$")
  expect_equal(tex_escape_math("`a %>% b` 50%"), "\\bqcode{a \\%>\\% b} 50\\%")
  expect_equal(tex_escape_math("`~x^2` `{}`"),
               "\\bqcode{\\textasciitilde{}x\\textasciicircum{}2} \\bqcode{\\{\\}}")
  expect_equal(tex_escape_math("one ` only"), "one ` only")
})

test_that("a code block is parsed out of the stem, verbatim", {
  dir <- code_fixture_course()
  blocks <- parse_question_blocks(file.path(dir, "questions.md"), test_config(1:2))
  q <- blocks[[1]]
  expect_equal(q$question, "Consider this code:")
  expect_equal(q$code[1], "flow |>")
  expect_equal(q$code[2], "  group_by(month) |>")
  expect_equal(q$code[3], "")
  expect_length(q$code, 6)
  expect_equal(q$tail, "What does `summarise()` return?")
  expect_equal(unname(q$options["a"]), "one row per `month`")
  expect_null(blocks[[2]]$code)
})

test_that("shuffling skips lines inside a code block", {
  dir <- code_fixture_course(); out <- file.path(dir, "output")
  cfg <- test_config(1:2)
  suppressMessages(key <- generate_versions(cfg, file.path(dir, "questions.md"), out))
  src <- readLines(file.path(dir, "questions.md"))
  for (v in 1:2) {
    lines <- readLines(file.path(out, sprintf("questions_v%d.md", v)))
    expect_true(all(src[3:10] %in% lines))   # code block unchanged
  }
  expect_equal(key$answer_master, c("A", "B"))
})

test_that("malformed code blocks stop with a clear message", {
  bad <- function(...) {
    p <- tempfile(fileext = ".md"); writeLines(c(...), p)
    parse_question_blocks(p, test_config(1))
  }
  opts <- c("a. 1", "b. 2", "c. 3", "d. 4", "e. 5")
  expect_error(bad("**Question 1:** q", "```", "x"), "no closing")
  expect_error(bad("**Question 1:** q", "```", "x", "```", "```", "y", "```", opts),
               "more than one code block")
  expect_error(bad("**Question 1:** q", "a. 1", "```", "x", "```", "b. 2"),
               "before the options")
})

test_that("code renders into both paper layouts", {
  dir <- code_fixture_course(); out <- file.path(dir, "output")
  cfg <- test_config(1:2)
  suppressMessages(generate_versions(cfg, file.path(dir, "questions.md"), out))
  blocks <- parse_question_blocks(file.path(out, "questions_v1.md"), cfg)
  for (tex in list(latex_question_paper(cfg, blocks, "1"), latex_inline_quiz(cfg, blocks, "1"))) {
    tex <- paste(tex, collapse = "\n")
    expect_match(tex, "\\usepackage{fancyvrb}", fixed = TRUE)
    expect_match(tex, "\\begin{Verbatim}[fontsize=\\small", fixed = TRUE)
    expect_match(tex, "  summarise(q = mean(flow_mm))   # 100% \"mean\" & $x$", fixed = TRUE)
    expect_match(tex, "What does \\bqcode{summarise()} return?\\par", fixed = TRUE)
    expect_match(tex, "\\bqcode{read\\_csv(\"data/x\\_y.csv\")} costing \\$5", fixed = TRUE)
  }
})

test_that("code questions compile with xelatex in both layouts", {
  skip_if(Sys.which("xelatex") == "", "xelatex not available")
  skip_if_not_installed("pdftools")
  dir <- code_fixture_course(); out <- file.path(dir, "output")
  cfg <- test_config(1:2)
  suppressMessages({
    generate_versions(cfg, file.path(dir, "questions.md"), out)
    pdfs <- c(render_papers(cfg, out), make_quiz_forms(cfg, out))
  })
  pdfs <- pdfs[grepl("\\.pdf$", pdfs)]
  expect_gt(length(pdfs), 0)
  text <- paste(pdftools::pdf_text(grep("quizform_v1", pdfs, value = TRUE)), collapse = " ")
  expect_match(text, "group_by(month)", fixed = TRUE)
  expect_match(text, "read_csv(\"data/x_y.csv\")", fixed = TRUE)
})
