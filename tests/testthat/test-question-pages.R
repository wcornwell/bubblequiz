# Which questions the marker reads from a given page of the form.

test_that("only the questions printed on a page are read from it", {
  cfg <- test_config()
  layout <- list(QUESTION_PAGE = c("1" = 1L, "2" = 1L, "3" = 2L, "4" = 2L))

  expect_equal(questions_on_page(cfg, layout, 1L), c(1L, 2L))
  expect_equal(questions_on_page(cfg, layout, 2L), c(3L, 4L))
})

test_that("an unknown page yields no questions rather than all of them", {
  cfg <- test_config()
  layout <- list(QUESTION_PAGE = c("1" = 1L, "2" = 1L, "3" = 2L, "4" = 2L))
  expect_length(questions_on_page(cfg, layout, 3L), 0L)
})

test_that("a form with no page information reads every question", {
  # This is the single-page case, and any layout calibrated before multi-page
  # support existed.
  cfg <- test_config()
  layout <- list(QUESTION_PAGE = c("1" = 1L, "2" = 1L, "3" = 1L, "4" = 1L))
  expect_equal(questions_on_page(cfg, layout, NA_integer_), cfg$questions)
  expect_equal(questions_on_page(cfg, layout, 1L), cfg$questions)
})

test_that("load_layout defaults a pre-multi-page layout to page 1", {
  path <- tempfile(fileext = ".R")
  writeLines(c(
    'ANSWER_X <- list(col1 = c(A = 0.1, B = 0.2, C = 0.3, D = 0.4, E = 0.5))',
    'QUESTION_COL <- c("1"="col1","2"="col1")',
    'QUESTION_Y <- c("1"=0.5,"2"=0.6)'
  ), path)

  layout <- load_layout(path)
  expect_equal(unname(layout$QUESTION_PAGE), c(1L, 1L))
  expect_equal(names(layout$QUESTION_PAGE), c("1", "2"))
})

test_that("load_layout keeps the page map when one is present", {
  path <- tempfile(fileext = ".R")
  writeLines(c(
    'ANSWER_X <- list(col1 = c(A = 0.1, B = 0.2, C = 0.3, D = 0.4, E = 0.5))',
    'QUESTION_COL <- c("1"="col1","2"="col1")',
    'QUESTION_Y <- c("1"=0.5,"2"=0.6)',
    'QUESTION_PAGE <- c("1"=1,"2"=2)'
  ), path)

  layout <- load_layout(path)
  expect_equal(unname(as.integer(layout$QUESTION_PAGE)), c(1L, 2L))
})
