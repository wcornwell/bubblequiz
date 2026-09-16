#!/usr/bin/env Rscript
# test_layout.R -- regression check that the config-derived layout reproduces the
# hand-written tables from the original BEES2041 bubble sheet, exactly.
#
# Run from the repo root:  Rscript tests/test_layout.R

if (file.exists("R/config.R")) {
  source("R/config.R")
} else {
  library(bubblequiz)
}

fixture <- if (file.exists("tests/bees2041.yml")) "tests/bees2041.yml" else "bees2041.yml"
cfg <- load_exam_config(fixture)

fails <- 0L
check <- function(label, got, want) {
  if (identical(got, want)) {
    message("PASS  ", label)
  } else {
    fails <<- fails + 1L
    message("FAIL  ", label)
    message("  got:  ", paste(capture.output(str(got)), collapse = " "))
    message("  want: ", paste(capture.output(str(want)), collapse = " "))
  }
}

# --- EXPECTED_QUESTIONS (was hard-coded in 4 separate files) ----------------
check("EXPECTED_QUESTIONS",
      as.integer(cfg$questions),
      as.integer(c(1:15, 17:21, 22:26)))

check("essay questions excluded", as.integer(cfg$essay_questions), c(16L, 27L))

# --- VALID_VERSIONS --------------------------------------------------------
check("VALID_VERSIONS", cfg$valid_versions, c("1", "2", "3", "4"))

# --- QUESTION_COL (was bubble_layout.R / bubble_calibrate_coords.R:43-52) ---
want_col <- c(
  "1"="left","2"="left","3"="left","4"="left","5"="left",
  "6"="right","7"="right","8"="right","9"="right","10"="right",
  "11"="left","12"="left","13"="left",
  "14"="right","15"="right",
  "17"="left","18"="left","19"="left",
  "20"="right","21"="right",
  "22"="left","23"="left","24"="left",
  "25"="right","26"="right"
)
got_col <- cfg$question_col[names(want_col)]
check("QUESTION_COL", got_col, want_col)

# --- ROW_QUESTION_MAP (was bubble_calibrate_coords.R:25-40) ----------------
want_rows <- list(
  c("1","6"), c("2","7"), c("3","8"), c("4","9"), c("5","10"),
  c("11","14"), c("12","15"), c("13"),
  c("17","20"), c("18","21"), c("19"),
  c("22","25"), c("23","26"), c("24")
)
got_rows <- lapply(cfg$bubble_rows, function(r) {
  qs <- unlist(r[intersect(cfg$col_names, names(r))])
  as.character(qs[qs %in% cfg$questions])
})
check("ROW_QUESTION_MAP (14 bubble rows)", got_rows, want_rows)

# --- Vision prompt sanity --------------------------------------------------
vp <- cfg$vision_prompt
check("prompt states 25 answer rows", grepl("Read all 25 answer rows", vp), TRUE)
check("prompt lists section ranges",
      grepl("Q1-Q10 \\(Section A\\), Q11-Q15 \\(Section B\\), Q17-Q21 \\(Section C\\), Q22-Q26 \\(Section D\\)", vp),
      TRUE)
check("prompt flags essay questions", grepl("Q16 and Q27 are essay questions", vp), TRUE)
check("prompt has no BEES2041-specific leftovers", grepl("BEES", vp), FALSE)

# --- Validation catches bad configs ----------------------------------------
expect_error <- function(label, expr) {
  ok <- inherits(try(expr, silent = TRUE), "try-error")
  check(label, ok, TRUE)
}
bad <- function(f) {
  cfg2 <- yaml::read_yaml(fixture)
  tmp <- tempfile(fileext = ".yml")
  yaml::write_yaml(f(cfg2), tmp)
  load_exam_config(tmp)
}
expect_error("rejects duplicate question numbers",
             bad(function(y) { y$sections[[1]]$questions <- c(y$sections[[1]]$questions, 3); y }))
expect_error("rejects question listed as both MCQ and essay",
             bad(function(y) { y$sections[[1]]$questions <- c(y$sections[[1]]$questions, 16); y }))
expect_error("rejects versions > 5",
             bad(function(y) { y$versions <- 6; y }))

if (fails > 0) {
  message("\n", fails, " check(s) failed.")
  quit(status = 1)
}
message("\nAll layout checks passed.")
