# A static 22-page fixture generated entirely from the worked example: eleven
# two-sided sheets with fake zIDs, known answers, and small scanner-like shifts
# and rotations. The marker must read every sheet exactly and flag none.

fixture <- function() {
  d <- test_path("fixtures", "synthetic-scan")
  skip_if_not(file.exists(file.path(d, "scan.pdf")), "synthetic scan fixture not present")
  d
}

fixture_run <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    skip_without_scan_tools()
    d <- fixture()
    work <- tempfile("synthetic-fixture-")
    dir.create(work)
    file.copy(file.path(d, "scan.pdf"), work)
    ex <- example_dir()
    cfg <- load_exam_config(file.path(ex, "exam.yml"))
    forms <- file.path(ex, "output")
    run <- run_pipeline(file.path(work, "scan.pdf"), cfg, forms,
                        file.path(forms, "answer_key.csv"))
    run$cfg <- cfg
    run$expected <- utils::read.csv(file.path(d, "expected.csv"), colClasses = "character")
    cache <<- run
    run
  }
})

test_that("all 22 pages pair into 11 sheets", {
  run <- fixture_run()
  expect_equal(nrow(run$sheets), 11)
})

test_that("every version is read from the page QR", {
  run <- fixture_run()
  expect_equal(as.character(run$sheets$exam_version), run$expected$version)
})

test_that("every zID is read exactly", {
  run <- fixture_run()
  expect_equal(run$sheets$zid, run$expected$zid)
})

test_that("every answer matches the generated marks", {
  run <- fixture_run()
  expect_equal(answers_string(run$sheets, run$cfg), run$expected$answers)
})

test_that("no sheet is flagged for review", {
  run <- fixture_run()
  expect_equal(run$sheets$notes[run$sheets$needs_review], character(0))
})
