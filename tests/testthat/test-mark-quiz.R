# One quiz, start to finish, the way it is run each week: scans in a folder,
# mark_quiz(), decide the flagged sheets in the review file, mark_quiz() again.

test_that("mark_quiz marks a folder, takes review decisions, and uploads", {
  skip_without_scan_tools()
  ex <- example_dir()
  cfg <- load_exam_config(file.path(ex, "exam.yml"))
  forms <- file.path(ex, "output")
  work <- withr::local_tempdir()
  quiz <- file.path(work, "week9")
  dir.create(quiz)
  jpg <- c(make_sheet(forms, cfg, "1", "9000001", c("A", "C", "A", "B", "A", "B"), work, 1),
           make_sheet(forms, cfg, "2", "9000002", c("C", "AE", "C", "E", "C", "E"), work, 2))
  write_scan_pdf(jpg, file.path(quiz, "scan.pdf"))

  run <- function() suppressWarnings(suppressMessages(utils::capture.output(
    res <- mark_quiz(quiz, config = cfg, forms = forms, grade_item = "Week 9 quiz"))))
  run()
  up <- utils::read.csv(file.path(quiz, "moodle_import.csv"), check.names = FALSE)
  expect_equal(up$Username, "z9000001")
  rv <- utils::read.csv(file.path(quiz, "review.csv"), colClasses = "character")
  expect_equal(rv$zid, "z9000002")
  expect_match(rv$reason, "Q2 two marks")

  rv$correct_answers <- "Q2=A"
  rv$comment <- "E crossed out"
  utils::write.csv(rv, file.path(quiz, "review.csv"), row.names = FALSE)
  t0 <- Sys.time()
  run()
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 60)  # not re-marked
  up <- utils::read.csv(file.path(quiz, "moodle_import.csv"), check.names = FALSE)
  expect_equal(up$Username, c("z9000001", "z9000002"))
  expect_equal(up[["Week 9 quiz"]], c(6, 6))
  rv <- utils::read.csv(file.path(quiz, "review.csv"), colClasses = "character")
  expect_equal(rv$status, "resolved")
  expect_equal(rv$comment, "E crossed out")
})
