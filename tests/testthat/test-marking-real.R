# Real scans: the first eleven sheets of a 2026 BEES1041 quiz, marked by hand,
# from a Fujifilm Apeos scanner at 200 dpi. De-identified -- names removed and
# each student's zID marks moved onto a random fake zID -- but otherwise the
# real pen and pencil: heavy fills, slashes, light pencil, margin ticks and
# crossed-out options. The marker must read every sheet exactly, and flag none.

fixture <- function() {
  d <- test_path("fixtures", "real-scan")
  skip_if_not(file.exists(file.path(d, "scan.pdf")), "real-scan fixture not present")
  d
}

real_run <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    skip_without_scan_tools()
    d <- fixture()
    work <- tempfile("real-")
    dir.create(work)
    file.copy(list.files(d, full.names = TRUE), work)
    cfg <- load_exam_config(file.path(work, "exam.yml"))
    run <- run_pipeline(file.path(work, "scan.pdf"), cfg, work,
                        file.path(work, "answer_key.csv"))
    run$cfg <- cfg
    run$expected <- utils::read.csv(file.path(d, "expected.csv"), colClasses = "character")
    cache <<- run
    run
  }
})

test_that("all 22 pages pair into 11 sheets", {
  run <- real_run()
  expect_equal(nrow(run$sheets), 11)
})

test_that("every version is read from the page QR", {
  run <- real_run()
  expect_equal(as.character(run$sheets$exam_version), run$expected$version)
})

test_that("every zID is read exactly", {
  run <- real_run()
  expect_equal(run$sheets$zid, run$expected$zid)
})

test_that("every answer matches the hand marking", {
  run <- real_run()
  expect_equal(answers_string(run$sheets, run$cfg), run$expected$answers)
})

test_that("no sheet is flagged for review", {
  run <- real_run()
  expect_equal(run$sheets$notes[run$sheets$needs_review], character(0))
})
