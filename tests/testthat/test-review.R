# From marked sheets to a gradebook upload: flags must survive scoring, a
# person's overrides must clear them, and only clean sheets may be uploaded.

# Three sheets of a four-question, two-version test. Sheet 1 is clean. Sheet 2
# has an uncertain Q2 (two marks). Sheet 3 has an unreadable zID digit.
review_dir <- function(overrides = NULL) {
  dir <- tempfile("bq-review-")
  dir.create(dir)
  sheets <- data.frame(
    sheet = 1:3, pages = 2L,
    files = c("page_0001.png;page_0002.png", "page_0003.png;page_0004.png",
              "page_0005.png;page_0006.png"),
    page = c(1L, 3L, 5L),
    file = c("page_0001.png", "page_0003.png", "page_0005.png"),
    status = "done",
    zid = c("z1111111", "z2222222", "z33?3333"),
    name = NA, exam_version = c("1", "2", "1"), confidence = "high",
    needs_review = c(FALSE, TRUE, TRUE),
    notes = c("", "Q2 two marks: B 115.0 and A 93.0 (one may be crossed out)",
              "zID digit 3 blank"),
    error = NA,
    q1 = c("A", "C", "A"), q2 = c("B", "B*", "B"),
    q3 = c("C", "A", "C"), q4 = c("D", "D", "D"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(sheets, file.path(dir, "sheets.csv"), row.names = FALSE)
  utils::write.csv(sheets, file.path(dir, "progress.csv"), row.names = FALSE)
  key <- data.frame(question = 1:4, answer_master = c("A", "B", "C", "D"),
                    answer_v1 = c("A", "B", "C", "D"), answer_v2 = c("C", "A", "A", "D"))
  utils::write.csv(key, file.path(dir, "key.csv"), row.names = FALSE)
  ov <- data.frame(file = character(), page = character(), zid = character(),
                   name = character(), question = character(), response = character())
  if (!is.null(overrides)) ov <- rbind(ov, overrides)
  utils::write.csv(ov, file.path(dir, "overrides.csv"), row.names = FALSE)
  dir
}

ov_row <- function(file = NA, zid = NA, question, response = NA) {
  data.frame(file = file, page = NA, zid = zid, name = NA,
             question = question, response = response, stringsAsFactors = FALSE)
}

score <- function(dir) {
  utils::capture.output(r <- suppressMessages(suppressWarnings(
    score_results(dir, key = file.path(dir, "key.csv"), config = test_config()))))
  r
}

test_that("flags from marking reach results.csv", {
  r <- score(review_dir())
  expect_equal(r$needs_review, c(FALSE, TRUE, TRUE))
  expect_match(r$notes[2], "two marks")
})

test_that("results.csv on disk carries the review columns", {
  dir <- review_dir()
  score(dir)
  written <- utils::read.csv(file.path(dir, "results.csv"))
  expect_true(all(c("needs_review", "reviewed", "notes") %in% names(written)))
})

test_that("overriding the uncertain answer clears the sheet", {
  r <- score(review_dir(ov_row(file = "page_0003.png", question = "2", response = "A")))
  expect_false(r$needs_review[2])
  expect_true(r$reviewed[2])
  expect_equal(r$score[2], 4)   # v2 key: C A A D
})

test_that("any page of a sheet identifies it", {
  r <- score(review_dir(ov_row(file = "page_0004.png", question = "2", response = "A")))
  expect_false(r$needs_review[2])
})

test_that("'ok' does not wave through an uncertain answer", {
  r <- score(review_dir(ov_row(file = "page_0003.png", question = "ok")))
  expect_true(r$needs_review[2])
  expect_match(r$notes[2], "still needs an override")
})

test_that("'ok' clears a sheet flagged for a reason that needs no change", {
  dir <- review_dir(ov_row(file = "page_0001.png", question = "ok"))
  s <- utils::read.csv(file.path(dir, "sheets.csv"))
  s$needs_review[1] <- TRUE
  s$notes[1] <- "sheet scanned back side first"
  utils::write.csv(s, file.path(dir, "sheets.csv"), row.names = FALSE)
  r <- score(dir)
  expect_false(r$needs_review[1])
})

test_that("a zID can be corrected, and must be valid", {
  r <- score(review_dir(ov_row(file = "page_0005.png", question = "zid", response = "z3333333")))
  expect_equal(r$zid[3], "z3333333")
  expect_false(r$needs_review[3])

  r <- score(review_dir(ov_row(file = "page_0005.png", question = "zid", response = "z333")))
  expect_equal(r$zid[3], "z33?3333")
  expect_true(r$needs_review[3])
})

test_that("an invalid zID is flagged even if marking did not flag it", {
  dir <- review_dir()
  s <- utils::read.csv(file.path(dir, "sheets.csv"))
  s$needs_review[3] <- FALSE
  utils::write.csv(s, file.path(dir, "sheets.csv"), row.names = FALSE)
  r <- score(dir)
  expect_true(r$needs_review[3])
})

test_that("preprocessing again keeps the reviewer's overrides", {
  skip_without_scan_tools()
  work <- withr::local_tempdir()
  form <- file.path(example_dir(), "output", "quizform_v1.pdf")
  pdf <- write_scan_pdf(c(form_page_jpeg(form, 1, file.path(work, "a.jpg")),
                          form_page_jpeg(form, 2, file.path(work, "b.jpg"))),
                        file.path(work, "scan.pdf"))
  cfg <- load_exam_config(file.path(example_dir(), "exam.yml"))
  suppressMessages(preprocess_scans(pdf, cfg, force = TRUE))
  ov <- file.path(work, "scan", "overrides.csv")
  utils::write.csv(ov_row(file = "page_0001.png", question = "ok"), ov, row.names = FALSE)
  suppressMessages(preprocess_scans(pdf, cfg, force = TRUE))
  expect_equal(nrow(utils::read.csv(ov)), 1)
})

test_that("only clean or reviewed sheets are exported", {
  dir <- review_dir(ov_row(file = "page_0003.png", question = "2", response = "A"))
  score(dir)
  out <- file.path(dir, "moodle.csv")
  res <- suppressMessages(export_moodle(dir, out, grade_item = "Week 2 quiz",
                                        config = test_config()))
  up <- utils::read.csv(out, check.names = FALSE)
  expect_equal(names(up), c("Username", "Week 2 quiz"))
  expect_equal(up$Username, c("z1111111", "z2222222"))
  expect_equal(up[["Week 2 quiz"]], c(4, 4))
  held <- utils::read.csv(sub("[.]csv$", "_to_review.csv", out))
  expect_equal(held$zid, "z33?3333")
  expect_match(held$reason, "zID")
})

test_that("export refuses a zID that appears on two sheets", {
  dir <- review_dir(ov_row(file = "page_0005.png", question = "zid", response = "z1111111"))
  score(dir)
  expect_error(suppressMessages(export_moodle(dir, file.path(dir, "m.csv"),
                                              config = test_config())),
               "more than one sheet")
  expect_false(file.exists(file.path(dir, "m.csv")))
})

test_that("export combines several scan folders", {
  a <- review_dir(); b <- review_dir()
  s <- utils::read.csv(file.path(b, "sheets.csv"))
  s$zid <- c("z4444444", "z5555555", "z6666666")
  utils::write.csv(s, file.path(b, "sheets.csv"), row.names = FALSE)
  score(a); score(b)
  res <- suppressMessages(export_moodle(c(a, b), file.path(a, "m.csv"), config = test_config()))
  expect_equal(sort(res$upload$Username), c("z1111111", "z4444444"))
  expect_equal(nrow(res$review), 4)
})
