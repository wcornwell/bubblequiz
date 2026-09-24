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

test_that("any page of a sheet identifies it, with or without its folder", {
  r <- score(review_dir(ov_row(file = "page_0004.png", question = "2", response = "A")))
  expect_false(r$needs_review[2])
  r <- score(review_dir(ov_row(file = "pages/page_0004.png", question = "2", response = "A")))
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

export <- function(dirs, out, ...) {
  suppressMessages(export_moodle(dirs, out, config = test_config(), ...))
}
review_of <- function(out) {
  utils::read.csv(sub("[.]csv$", "_to_review.csv", out), colClasses = "character",
                  check.names = FALSE)
}

test_that("only clean or reviewed sheets are exported", {
  dir <- review_dir(ov_row(file = "page_0003.png", question = "2", response = "A"))
  score(dir)
  out <- file.path(dir, "moodle.csv")
  export(dir, out, grade_item = "Week 2 quiz")
  up <- utils::read.csv(out, check.names = FALSE)
  expect_equal(names(up), c("Username", "Week 2 quiz"))
  expect_equal(up$Username, c("z1111111", "z2222222"))
  expect_equal(up[["Week 2 quiz"]], c(4, 4))
  rv <- review_of(out)
  expect_equal(rv$zid, c("z2222222", "z33?3333"))
  expect_equal(rv$status, c("resolved", "open"))
  expect_equal(rv$files[2], "page_0005.png;page_0006.png")  # every page, as overrides name them
  expect_equal(rv$answers[2], "A B C D")
  expect_match(rv$reason[2], "zID")
})

test_that("decisions written in the review file are applied", {
  dir <- review_dir()
  out <- file.path(dir, "moodle.csv")
  score(dir); export(dir, out)
  rv <- review_of(out)
  expect_equal(rv$status, c("open", "open"))
  rv$correct_answers[1] <- "Q2=A"
  rv$correct_zid[2] <- "3333333"          # prefix optional
  rv$comment <- c("B crossed out", "digit 3 read from handwriting")
  utils::write.csv(rv, sub("[.]csv$", "_to_review.csv", out), row.names = FALSE)

  r <- utils::capture.output(res <- suppressMessages(score_results(
    dir, key = file.path(dir, "key.csv"), config = test_config(),
    review = sub("[.]csv$", "_to_review.csv", out))))
  export(dir, out)
  up <- utils::read.csv(out)
  expect_equal(up$Username, c("z1111111", "z2222222", "z3333333"))
  rv <- review_of(out)
  expect_equal(rv$status, c("resolved", "resolved"))
  expect_equal(rv$comment, c("B crossed out", "digit 3 read from handwriting"))
  expect_equal(rv$zid[2], "z3333333")
})

test_that("a re-run never drops the reviewer's rows, decisions or own columns", {
  dir <- review_dir()
  out <- file.path(dir, "moodle.csv")
  score(dir); export(dir, out)
  rv <- review_of(out)
  rv$`manual comments` <- c("looked at this", "")
  rv$comment[2] <- "ask the student"
  utils::write.csv(rv, sub("[.]csv$", "_to_review.csv", out), row.names = FALSE)
  export(dir, out); export(dir, out)
  rv2 <- review_of(out)
  expect_equal(nrow(rv2), 2)
  expect_equal(rv2$`manual comments`, c("looked at this", ""))
  expect_equal(rv2$comment[2], "ask the student")
})

test_that("resolved = yes signs off a sheet that needs no change", {
  dir <- review_dir()
  s <- utils::read.csv(file.path(dir, "sheets.csv"))
  s$needs_review[1] <- TRUE; s$notes[1] <- "sheet scanned back side first"
  utils::write.csv(s, file.path(dir, "sheets.csv"), row.names = FALSE)
  out <- file.path(dir, "moodle.csv")
  score(dir); export(dir, out)
  rv <- review_of(out)
  rv$resolved[rv$zid == "z1111111"] <- "yes"
  review <- sub("[.]csv$", "_to_review.csv", out)
  utils::write.csv(rv, review, row.names = FALSE)
  utils::capture.output(suppressMessages(score_results(dir, key = file.path(dir, "key.csv"),
                                                       config = test_config(), review = review)))
  expect_true("z1111111" %in% export(dir, out)$upload$Username)
})

test_that("a malformed decision stops the run and names the row", {
  dir <- review_dir()
  out <- file.path(dir, "moodle.csv")
  score(dir); export(dir, out)
  review <- sub("[.]csv$", "_to_review.csv", out)
  rv <- review_of(out)
  for (bad in list(c(correct_answers = "Q2 is A"), c(correct_answers = "Q9=A"),
                   c(correct_answers = "Q2=Z"), c(correct_zid = "z12"),
                   c(resolved = "maybe"))) {
    x <- rv; x[[names(bad)]][1] <- bad[[1]]
    utils::write.csv(x, review, row.names = FALSE)
    expect_error(score_results(dir, key = file.path(dir, "key.csv"),
                               config = test_config(), review = review), "review file row 2")
  }
})

test_that("export warns when decisions have not been scored yet", {
  dir <- review_dir()
  out <- file.path(dir, "moodle.csv")
  score(dir); export(dir, out)
  rv <- review_of(out); rv$correct_answers[1] <- "Q2=A"
  utils::write.csv(rv, sub("[.]csv$", "_to_review.csv", out), row.names = FALSE)
  expect_warning(export(dir, out), "not in the scores yet")
})

test_that("export refuses a zID that appears on two sheets", {
  dir <- review_dir(ov_row(file = "page_0005.png", question = "zid", response = "z1111111"))
  score(dir)
  expect_error(export(dir, file.path(dir, "m.csv")), "more than one sheet")
  expect_false(file.exists(file.path(dir, "m.csv")))
})

test_that("export combines several scan folders", {
  a <- review_dir(); b <- review_dir()
  s <- utils::read.csv(file.path(b, "sheets.csv"))
  s$zid <- c("z4444444", "z5555555", "z6666666")
  utils::write.csv(s, file.path(b, "sheets.csv"), row.names = FALSE)
  score(a); score(b)
  res <- export(c(a, b), file.path(a, "m.csv"))
  expect_equal(sort(res$upload$Username), c("z1111111", "z4444444"))
  expect_equal(nrow(res$review), 4)
})

test_that("review decisions work alongside an empty old-style overrides.csv", {
  # Scan folders made before overrides.csv had a `file` column.
  dir <- review_dir()
  writeLines("page,zid,name,question,response", file.path(dir, "overrides.csv"))
  out <- file.path(dir, "moodle.csv")
  score(dir); export(dir, out)
  rv <- review_of(out); rv$correct_answers[1] <- "Q2=A"
  review <- sub("[.]csv$", "_to_review.csv", out)
  utils::write.csv(rv, review, row.names = FALSE)
  utils::capture.output(r <- suppressMessages(score_results(
    dir, key = file.path(dir, "key.csv"), config = test_config(), review = review)))
  expect_false(r$needs_review[2])
})

test_that("resolved = exclude leaves a sheet out for good", {
  dir <- review_dir()
  out <- file.path(dir, "moodle.csv")
  score(dir); export(dir, out)
  review <- sub("[.]csv$", "_to_review.csv", out)
  rv <- review_of(out); rv$resolved[rv$zid == "z33?3333"] <- "exclude"
  utils::write.csv(rv, review, row.names = FALSE)
  utils::capture.output(suppressMessages(score_results(dir, key = file.path(dir, "key.csv"),
                                                       config = test_config(), review = review)))
  res <- export(dir, out)
  expect_false(any(grepl("\\?", res$upload$Username)))
  expect_equal(review_of(out)$status[review_of(out)$zid == "z33?3333"], "excluded")
})

test_that("a review file saved from a stale copy is caught, and every version kept", {
  dir <- review_dir()
  out <- file.path(dir, "moodle.csv")
  review <- sub("[.]csv$", "_to_review.csv", out)
  score(dir); export(dir, out)
  stale <- utils::read.csv(review, colClasses = "character", check.names = FALSE)  # opened now...
  rv <- stale; rv$correct_answers[1] <- "Q2=A"
  utils::write.csv(rv, review, row.names = FALSE)
  utils::capture.output(suppressMessages(score_results(dir, key = file.path(dir, "key.csv"),
                                                       config = test_config(), review = review)))
  export(dir, out)                                   # ...the pipeline writes the decision back
  stale$`manual comments` <- c("new note", "")
  utils::write.csv(stale, review, row.names = FALSE)  # ...then the stale copy is saved over it
  lost <- lost_decisions(review)
  expect_equal(nrow(lost), 1)
  expect_equal(lost$was, "Q2=A")
  expect_warning(export(dir, out), "now empty")
  hist <- list.files(file.path(dir, ".review_history"))
  expect_gte(sum(grepl("_to_review_[0-9-]+[.]csv$", hist)), 2)
})
