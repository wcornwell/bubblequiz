# Deciding what a row of bubble scores means. The rule: accept a mark only when
# it is clearly dark and clearly darker than everything else in its row; flag
# anything in between for a person rather than guess.

scores <- function(...) c(...)

test_that("one clear mark is read", {
  cls <- classify_bubble_row(scores(A = 2, B = 60, C = 1, D = 4, E = 0), ANSWER_READ)
  expect_equal(cls$answer, "B")
  expect_false(cls$uncertain)
})

test_that("an empty row is blank and flagged", {
  cls <- classify_bubble_row(scores(A = 2, B = 5, C = 1, D = 4, E = 0), ANSWER_READ)
  expect_equal(cls$answer, "")
  expect_true(cls$uncertain)
})

test_that("a faint mark is flagged, not accepted", {
  cls <- classify_bubble_row(scores(A = 2, B = 14, C = 1, D = 4, E = 0), ANSWER_READ)
  expect_equal(cls$answer, "B*")
  expect_true(cls$uncertain)
  expect_match(cls$note, "faint")
})

test_that("two marks are flagged, not resolved to the darker", {
  cls <- classify_bubble_row(scores(A = 2, B = 60, C = 55, D = 4, E = 0), ANSWER_READ)
  expect_equal(cls$answer, "B*")
  expect_true(cls$uncertain)
  expect_match(cls$note, "two marks")
})

test_that("a crossed-out correction is flagged even when one mark is far darker", {
  # Real scores from a scanned sheet: A filled then crossed out, B filled.
  # The crossed-out A is darker, and clears the gap rule; it must not win.
  cls <- classify_bubble_row(scores(A = 115.2, B = 93.2, C = 3, D = 2, E = 1), ANSWER_READ)
  expect_equal(cls$answer, "A*")
  expect_true(cls$uncertain)
  expect_match(cls$note, "two marks")
  # The same in a zID column: 4 filled and crossed out, 3 filled.
  z <- c(`0` = 5, `1` = 4, `2` = 6, `3` = 93.9, `4` = 106.8, `5` = 3, `6` = 8,
         `7` = 2, `8` = 7, `9` = 4)
  expect_true(classify_bubble_row(z, ZID_READ)$uncertain)
})

test_that("unreadable scores are flagged", {
  cls <- classify_bubble_row(scores(A = NA, B = 60), ANSWER_READ)
  expect_true(cls$uncertain)
})

test_that("the thresholds leave room on both sides of what real scans produced", {
  # Measured on real scans: filled answers >= 34.8, empty <= -0.5; filled zID
  # bubbles >= 30.6, empty <= 12.6.
  expect_lt(ANSWER_READ$mark, 34.8); expect_gt(ANSWER_READ$blank, -0.5)
  expect_lt(ZID_READ$mark, 30.6);    expect_gt(ZID_READ$mark, 12.6)
  expect_gt(ZID_READ$blank, 12.6)
})
