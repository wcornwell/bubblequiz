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

course <- tempfile("bubblequiz-course-")
dir.create(course)

exam <- c(
  "course: TEST1010",
  "title: \"Tracking Test\"",
  "date: \"1 Jan 2026\"",
  "versions: 4",
  "options: [A, B, C, D, E]",
  "id:",
  "  label: zID",
  "  prefix: z",
  "  digits: 7",
  "layout:",
  "  columns: 2",
  "sections:",
  "  - id: A",
  "    title: \"Lecture\"",
  "    questions: [1, 2]",
  "    marks_each: 1"
)
writeLines(exam, file.path(course, "exam.yml"))

questions <- c(
  "# Lecture quiz",
  "",
  "**Question 1 [1 mark]:** Which claim follows from the lecture?",
  "",
  "a. Alpha",
  "b. Bravo",
  "c. Charlie",
  "d. Delta",
  "e. Echo",
  "",
  "<!-- Answer A -->",
  "",
  "**Question 2 [1 mark]:** Which method was recommended?",
  "",
  "a. Alpha",
  "b. Bravo",
  "c. Charlie",
  "d. Delta",
  "e. Echo",
  "",
  "<!-- Answer C -->"
)
writeLines(questions, file.path(course, "questions.md"))

old <- setwd(course)
on.exit(setwd(old), add = TRUE)

cfg <- load_exam_config("exam.yml")
key <- generate_versions(cfg, "questions.md", "output")

check("answer key columns", names(key),
      c("question", "answer_master", "answer_v1", "answer_v2", "answer_v3", "answer_v4"))
check("v1 preserves master answers", as.character(key$answer_v1), c("A", "C"))
check("v2 remaps master answers", as.character(key$answer_v2), c("C", "A"))

dir.create("responses")
progress <- data.frame(
  status = c("done", "done"),
  api_call_ok = TRUE,
  file = c("pages/page_0001.jpeg", "pages/page_0002.jpeg"),
  page = c(1L, 2L),
  zid = c("z1111111", "z2222222"),
  name = c("Version One", "Version Two"),
  needs_review = FALSE,
  exam_version = c("1", "2"),
  confidence = "high",
  notes = "",
  error = NA_character_,
  q1 = c("A", "C"),
  q2 = c("C", "A"),
  stringsAsFactors = FALSE
)
readr::write_csv(progress, "responses/progress.csv")
readr::write_csv(data.frame(page = character(), zid = character(), name = character(),
                            question = integer(), response = character()),
                 "responses/overrides.csv")

results <- score_results("responses", "output/answer_key.csv", cfg)
check("version-specific score", as.integer(results$score), c(2L, 2L))

progress$q2[2] <- "C"
readr::write_csv(progress, "responses/progress.csv")
results <- score_results("responses", "output/answer_key.csv", cfg)
check("wrong answer on v2 uses v2 key", as.integer(results$score), c(2L, 1L))

progress$exam_version[2] <- NA_character_
readr::write_csv(progress, "responses/progress.csv")
results <- suppressWarnings(score_results("responses", "output/answer_key.csv", cfg))
check("missing version is not guessed", is.na(results$score[2]), TRUE)

if (fails > 0) {
  message("\n", fails, " check(s) failed.")
  quit(status = 1)
}
message("\nAll version/scoring checks passed.")
