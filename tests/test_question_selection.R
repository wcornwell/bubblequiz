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

course <- tempfile("bubblequiz-select-")
dir.create(course)
candidates <- file.path(course, "question_candidates.md")
output <- file.path(course, "questions.md")

writeLines(c(
  "# Candidate bank",
  "",
  "**Question 1 [1 mark]:** First candidate?",
  "",
  "a. Alpha",
  "b. Bravo",
  "c. Charlie",
  "d. Delta",
  "e. Echo",
  "",
  "<!-- Answer A -- first rationale -->",
  "",
  "**Question 2 [1 mark]:** Second candidate?",
  "",
  "a. Alpha",
  "b. Bravo",
  "c. Charlie",
  "d. Delta",
  "e. Echo",
  "",
  "<!-- Answer B -- second rationale -->",
  "",
  "**Question 3 [1 mark]:** Third candidate?",
  "",
  "a. Alpha",
  "b. Bravo",
  "c. Charlie",
  "d. Delta",
  "e. Echo",
  "",
  "<!-- Answer C -- third rationale -->"
), candidates)

selected <- select_questions(candidates, selected = "3,1", output = output)
lines <- readLines(output, warn = FALSE)

check("selected ids returned", selected, c(3L, 1L))
check("selected count", sum(grepl("^\\*\\*Question", lines)), 2L)
check("renumbered headings",
      grep("^\\*\\*Question", lines, value = TRUE),
      c("**Question 1 [1 mark]:** Third candidate?",
        "**Question 2 [1 mark]:** First candidate?"))
check("answers preserved",
      grep("^<!-- Answer", lines, value = TRUE),
      c("<!-- Answer C -- third rationale -->",
        "<!-- Answer A -- first rationale -->"))

if (fails > 0) {
  message("\n", fails, " check(s) failed.")
  quit(status = 1)
}
message("\nAll question-selection checks passed.")
