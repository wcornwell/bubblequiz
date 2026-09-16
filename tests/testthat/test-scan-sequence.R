# Grouping a scanned stack into sheets. These exercise sequence_walk() directly
# so the logic is tested without needing scans or a QR decoder.

two <- function(n) rep(2L, n)

test_that("a clean duplex stack groups into whole sheets", {
  w <- sequence_walk(c(1L, 2L, 1L, 2L, 1L, 2L), two(6),
                     c("1", "1", "3", "3", "2", "2"))
  expect_equal(w$sheet, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_equal(unique(w$status), "ok")
})

test_that("a swallowed page is flagged and does not cascade", {
  # Sheet 2's front never made it through the feeder.
  w <- sequence_walk(c(1L, 2L, 2L, 1L, 2L), two(5),
                     c("1", "1", "3", "2", "2"))
  expect_true(is.na(w$sheet[3]))
  expect_match(w$status[3], "^orphan page 2")
  # Everything after the error still groups correctly.
  expect_equal(w$sheet[4:5], c(2L, 2L))
  expect_equal(w$status[4:5], c("ok", "ok"))
})

test_that("a sheet fed back-to-front is not silently paired", {
  w <- sequence_walk(c(2L, 1L, 1L, 2L), two(4), c("1", "1", "4", "4"))
  expect_true(is.na(w$sheet[1]))
  expect_match(w$status[1], "^orphan")
})

test_that("two versions are never paired into one sheet", {
  w <- sequence_walk(c(1L, 2L), two(2), c("1", "4"))
  expect_true(is.na(w$sheet[1]))
  expect_match(w$status[1], "version changes within sheet")
})

test_that("an unreadable page is reported rather than skipped", {
  w <- sequence_walk(c(1L, NA_integer_, 1L, 2L), two(4),
                     c("1", NA_character_, "2", "2"))
  expect_equal(w$status[2], "unreadable QR")
  expect_true(is.na(w$sheet[2]))
})

test_that("a stack that stops mid-sheet is caught", {
  w <- sequence_walk(c(1L, 2L, 1L), two(3), c("1", "1", "2"))
  expect_match(w$status[3], "stack ends mid-sheet")
  expect_true(is.na(w$sheet[3]))
})

test_that("a single-page form is one sheet per page", {
  w <- sequence_walk(c(1L, 1L, 1L), rep(1L, 3), c("1", "2", "1"))
  expect_equal(w$sheet, c(1L, 2L, 3L))
  expect_equal(unique(w$status), "ok")
})
