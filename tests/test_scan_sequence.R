#!/usr/bin/env Rscript

if (dir.exists("R")) {
  for (f in list.files("R", "[.]R$", full.names = TRUE)) source(f)
} else {
  library(bubblequiz)
}

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

two <- function(n) rep(2L, n)

# A clean duplex stack groups into whole sheets.
w <- sequence_walk(c(1L, 2L, 1L, 2L, 1L, 2L), two(6),
                   c("1", "1", "3", "3", "2", "2"))
check("clean stack groups into sheets", w$sheet, c(1L, 1L, 2L, 2L, 3L, 3L))
check("clean stack is all ok", unique(w$status), "ok")

# A swallowed page is flagged, and the stack resynchronises after it.
w <- sequence_walk(c(1L, 2L, 2L, 1L, 2L), two(5),
                   c("1", "1", "3", "2", "2"))
check("swallowed page is not grouped", w$sheet, c(1L, 1L, NA_integer_, 2L, 2L))
check("swallowed page is flagged as orphan",
      grepl("^orphan page 2", w$status[3]), TRUE)
check("pages after the error still group", w$status[4:5], c("ok", "ok"))

# A sheet fed back-to-front does not silently pair up.
w <- sequence_walk(c(2L, 1L, 1L, 2L), two(4), c("1", "1", "4", "4"))
check("reversed sheet is not grouped as a sheet", is.na(w$sheet[1]), TRUE)
check("reversed sheet flags the leading page",
      grepl("^orphan", w$status[1]), TRUE)

# Two different versions must never be paired into one sheet.
w <- sequence_walk(c(1L, 2L), two(2), c("1", "4"))
check("version change within a sheet is rejected", is.na(w$sheet[1]), TRUE)
check("version change is explained",
      grepl("version changes within sheet", w$status[1]), TRUE)

# An undecodable page is reported, not skipped over silently.
w <- sequence_walk(c(1L, NA_integer_, 1L, 2L), two(4),
                   c("1", NA_character_, "2", "2"))
check("unreadable page is flagged", w$status[2], "unreadable QR")
check("unreadable page has no sheet", is.na(w$sheet[2]), TRUE)

# A stack that stops mid-sheet is caught rather than half-marked.
w <- sequence_walk(c(1L, 2L, 1L), two(3), c("1", "1", "2"))
check("truncated final sheet is flagged",
      grepl("stack ends mid-sheet", w$status[3]), TRUE)

if (fails > 0) {
  message("\n", fails, " scan-sequence check(s) failed.")
  quit(status = 1)
}
message("\nAll scan-sequence checks passed.")
