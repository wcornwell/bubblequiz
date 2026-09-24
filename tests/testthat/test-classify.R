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
  cls <- classify_bubble_row(scores(A = 2, B = 20, C = 1, D = 4, E = 0), ANSWER_READ)
  expect_equal(cls$answer, "B*")
  expect_true(cls$uncertain)
  expect_match(cls$note, "faint")
})

test_that("two marks are flagged, not resolved to the darker", {
  cls <- classify_bubble_row(scores(A = 2, B = 60, C = 55, D = 4, E = 0), ANSWER_READ)
  expect_equal(cls$answer, "B*")
  expect_true(cls$uncertain)
  expect_match(cls$note, "ambiguous")
})

test_that("unreadable scores are flagged", {
  cls <- classify_bubble_row(scores(A = NA, B = 60), ANSWER_READ)
  expect_true(cls$uncertain)
})

test_that("the thresholds leave room on both sides of what real scans produced", {
  # Measured on real scans: filled answers >= 44.7, empty <= 9.2; filled zID
  # bubbles >= 37.9, empty <= 27.2, filled beating its column by >= 26.
  expect_lt(ANSWER_READ$mark, 44.7); expect_gt(ANSWER_READ$blank, 9.2)
  expect_lt(ZID_READ$mark, 37.9);    expect_gt(ZID_READ$mark, 27.2)
  expect_lt(ZID_READ$gap, 26)
})
