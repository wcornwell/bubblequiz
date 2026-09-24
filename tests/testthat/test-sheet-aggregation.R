# Joining the pages of a multi-page form back into one record per student.

test_that("pages of a sheet become one row with answers from both sides", {
  cfg <- test_config()
  dir <- scan_dir_with(two_page_progress())
  out <- suppressMessages(aggregate_sheets(dir, config = cfg))

  expect_equal(nrow(out), 2L)
  expect_equal(out$pages, c(2L, 2L))
  expect_equal(out$zid, c("z1111111", "z2222222"))
  expect_equal(as.character(out$exam_version), c("1", "2"))
  expect_equal(as.character(unlist(out[1, c("q1", "q2", "q3", "q4")])),
               c("A", "B", "C", "D"))
  expect_equal(as.character(unlist(out[2, c("q1", "q2", "q3", "q4")])),
               c("B", "C", "D", "E"))
})

test_that("a page the sequence check could not attribute is dropped, not scored", {
  cfg <- test_config()
  p <- two_page_progress()
  p$sheet[2] <- NA_integer_          # back of sheet 1 could not be placed
  dir <- scan_dir_with(p)

  expect_warning(out <- suppressMessages(aggregate_sheets(dir, config = cfg)),
                 "could not be attributed")
  expect_equal(nrow(out), 2L)
  expect_equal(out$pages[1], 1L)
  # The missing page's questions stay NA rather than being recorded as blank.
  expect_true(is.na(out$q3[1]))
  expect_true(is.na(out$q4[1]))
})

test_that("a sheet short of a page is flagged for review", {
  cfg <- test_config()
  p <- two_page_progress()
  p$sheet[2] <- NA_integer_
  dir <- scan_dir_with(p)

  out <- suppressWarnings(suppressMessages(aggregate_sheets(dir, config = cfg)))
  expect_true(out$needs_review[1])
  expect_match(out$notes[1], "incomplete sheet: 1 of 2 pages")
})

test_that("pages disagreeing on version are flagged rather than guessed", {
  cfg <- test_config()
  p <- two_page_progress()
  p$exam_version[2] <- "2"
  dir <- scan_dir_with(p)

  out <- suppressMessages(aggregate_sheets(dir, config = cfg))
  expect_match(out$notes[1], "version disagreement")
  expect_true(out$needs_review[1])
})

test_that("conflicting zIDs within a sheet are flagged", {
  cfg <- test_config()
  p <- two_page_progress()
  p$zid[2] <- "z9999999"
  dir <- scan_dir_with(p)

  out <- suppressMessages(aggregate_sheets(dir, config = cfg))
  expect_match(out$notes[1], "conflicting zIDs")
  expect_true(out$needs_review[1])
})

test_that("a sheet with no zID at all is flagged", {
  cfg <- test_config()
  p <- two_page_progress()
  p$zid <- NA_character_
  dir <- scan_dir_with(p)

  out <- suppressMessages(aggregate_sheets(dir, config = cfg))
  expect_true(all(grepl("no zID read", out$notes)))
  expect_true(all(out$needs_review))
})

test_that("a single-page form passes through unchanged", {
  cfg <- test_config()
  p <- two_page_progress()[c(1, 3), ]
  p$sheet <- NA_integer_
  p$q3 <- c("C", "D"); p$q4 <- c("D", "E")
  dir <- scan_dir_with(p)

  out <- suppressMessages(aggregate_sheets(dir, config = cfg))
  expect_equal(nrow(out), 2L)
  expect_equal(as.character(unlist(out[1, c("q1", "q2", "q3", "q4")])),
               c("A", "B", "C", "D"))
})

test_that("missing question columns are an error, not a silent NA", {
  cfg <- test_config()
  p <- two_page_progress()
  p$q4 <- NULL
  dir <- scan_dir_with(p)

  expect_error(suppressMessages(aggregate_sheets(dir, config = cfg)),
               "missing question column")
})

test_that("a note on every page of a sheet appears once", {
  p <- two_page_progress()
  p$notes <- c("sheet scanned back side first", "sheet scanned back side first | Q3 blank",
               NA, NA)
  dir <- scan_dir_with(p)
  out <- suppressMessages(aggregate_sheets(dir, config = test_config()))
  expect_equal(out$notes[1], "sheet scanned back side first | Q3 blank")
})
