# The whole pipeline -- render, sequence check, marking, aggregation, scoring --
# on sheets generated from the example forms, where every mark is known.

# One stack covers every case; building and marking it is the slow part, so it
# is done once and shared by the tests below.
synthetic_run <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    skip_without_scan_tools()
    ex <- example_dir()
    cfg <- load_exam_config(file.path(ex, "exam.yml"))
    forms <- file.path(ex, "output")
    work <- tempfile("synthetic-")
    dir.create(work)
    sheets <- list(
      # clean, different versions, ordinary scanner shift
      list(version = "1", zid = "9000001", answers = c("A", "C", "A", "B", "A", "B"),
           distort = list(dx_mm = 1, dy_mm = 0.5)),
      list(version = "3", zid = "9123456", answers = c("D", "B", "D", "C", "D", "C"),
           distort = list(dx_mm = -2, dy_mm = 1.5, deg = 0.4)),
      # fed through the scanner upside down
      list(version = "2", zid = "9765432", answers = c("C", "A", "C", "E", "C", "E"),
           distort = list(dx_mm = 0.5), upside_down = TRUE),
      # a double mark on Q2 and a blank Q5
      list(version = "4", zid = "9555555", answers = c("E", "DB", "E", "A", "", "A"),
           distort = list(dy_mm = -1)),
      # a zID column left empty
      list(version = "1", zid = "90?0002", answers = c("B", "B", "B", "B", "B", "B"),
           distort = list()),
      # every mark faint
      list(version = "2", zid = "9111111", answers = c("A", "A", "A", "A", "A", "A"),
           shade = 192, distort = list()),
      # put in the feeder the wrong way over: back side scanned first
      list(version = "3", zid = "9246810", answers = c("D", "B", "D", "C", "D", "C"),
           distort = list(dx_mm = 0.5), back_first = TRUE)
    )
    jpgs <- unlist(lapply(seq_along(sheets), function(i) {
      s <- sheets[[i]]
      p <- make_sheet(forms, cfg, s$version, s$zid, s$answers, work, i,
                      shade = s$shade %||% 40, distort = s$distort,
                      upside_down = isTRUE(s$upside_down))
      if (isTRUE(s$back_first)) rev(p) else p
    }))
    pdf <- write_scan_pdf(jpgs, file.path(work, "scan.pdf"))
    run <- run_pipeline(pdf, cfg, forms, file.path(forms, "answer_key.csv"))
    run$cfg <- cfg
    run$truth <- sheets
    cache <<- run
    run
  }
})

test_that("every page is grouped into its sheet", {
  run <- synthetic_run()
  expect_equal(nrow(run$sheets), length(run$truth))
  expect_equal(run$sheets$exam_version,
               vapply(run$truth, `[[`, "", "version"))
})

test_that("clean sheets are read exactly and not flagged", {
  run <- synthetic_run()
  for (i in 1:2) {
    t <- run$truth[[i]]
    expect_equal(run$sheets$zid[i], paste0("z", t$zid))
    expect_equal(answers_string(run$sheets, run$cfg)[i], paste(t$answers, collapse = ""))
    expect_false(run$sheets$needs_review[i])
  }
})

test_that("an upside-down sheet is turned round and read", {
  run <- synthetic_run()
  t <- run$truth[[3]]
  expect_equal(run$sheets$zid[3], paste0("z", t$zid))
  expect_equal(answers_string(run$sheets, run$cfg)[3], paste(t$answers, collapse = ""))
  expect_false(run$sheets$needs_review[3])
})

test_that("double marks and blanks are flagged, never guessed", {
  run <- synthetic_run()
  expect_true(run$sheets$needs_review[4])
  expect_match(run$sheets$q2[4], "\\*$")          # recorded as uncertain
  expect_true(is.na(run$sheets$q5[4]) || run$sheets$q5[4] == "")
  expect_match(run$sheets$notes[4], "Q2 two marks")
  expect_match(run$sheets$notes[4], "Q5 blank")
  # The questions around them are still read.
  expect_equal(run$sheets$q1[4], "E")
  expect_equal(run$sheets$q6[4], "A")
})

test_that("an empty zID column is flagged, not filled in", {
  run <- synthetic_run()
  expect_equal(run$sheets$zid[5], "z90?0002")
  expect_true(run$sheets$needs_review[5])
})

test_that("faint marks are flagged rather than accepted or dropped", {
  run <- synthetic_run()
  expect_true(run$sheets$needs_review[6])
  expect_match(run$sheets$notes[6], "faint")
})

test_that("scores follow the version-specific key", {
  run <- synthetic_run()
  # Sheets 1-3 are answered exactly to their version's key.
  expect_equal(run$results$score[1:3], c(6, 6, 6))
})

test_that("a sheet scanned back side first is read, and flagged to confirm", {
  run <- synthetic_run()
  t <- run$truth[[7]]
  expect_equal(run$sheets$zid[7], paste0("z", t$zid))
  expect_equal(answers_string(run$sheets, run$cfg)[7], paste(t$answers, collapse = ""))
  expect_true(run$sheets$needs_review[7])
  expect_match(run$sheets$notes[7], "back side first")
})
