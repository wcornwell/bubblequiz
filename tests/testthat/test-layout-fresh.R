# The marker must never read against a layout older than the forms.

test_that("a layout older than its forms is refused", {
  work <- withr::local_tempdir()
  layout <- file.path(work, "layout.R")
  writeLines(c("# Bubble coordinates -- GENERATED", "# Source: quizform_v1.pdf",
               "ANSWER_X <- list(col1 = c(A = 0.1))", 'QUESTION_COL <- c("1"="col1")',
               'QUESTION_Y <- c("1"=0.5)'), layout)
  form <- file.path(work, "quizform_v1.pdf")
  file.copy(file.path(example_dir(), "output", "quizform_v1.pdf"), form)
  Sys.setFileTime(layout, Sys.time() - 3600)
  expect_error(check_layout_fresh(layout), "older than")
  Sys.setFileTime(form, Sys.time() - 7200)
  expect_true(check_layout_fresh(layout))
})

test_that("the forms are used in preference to layout.R when both are present", {
  cfg <- load_exam_config(file.path(example_dir(), "exam.yml"))
  forms <- file.path(example_dir(), "output")
  lays <- suppressMessages(resolve_layouts(cfg, file.path(forms, "layout.R")))
  expect_true(is_layout_set(lays))
  expect_named(lays, cfg$valid_versions)
  # Versions differ in line breaks, so their answer rows sit at different heights.
  ys <- vapply(lays, function(l) l$QUESTION_Y[["1"]], numeric(1))
  expect_true(all(ys > 0 & ys < 1))
})

test_that("a missing form is an error when forms are requested explicitly", {
  cfg <- load_exam_config(file.path(example_dir(), "exam.yml"))
  expect_error(resolve_layouts(cfg, "nowhere/layout.R", forms = withr::local_tempdir()),
               "not found")
})
