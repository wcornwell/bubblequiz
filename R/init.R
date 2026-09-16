# init.R -- scaffold the editable files a course repository needs.

default_exam_yml <- function() {
  c(
    "# bubblequiz exam configuration -- edit this file for your course.",
    "",
    "course: COURSE1010",
    "title: \"COURSE1010 Final Exam -- Part 1\"",
    "date: \"TODO -- exam date\"",
    "subtitle: \"Multiple Choice Answer Sheet\"",
    "duration: \"90 min\"",
    "total_marks: 50",
    "",
    "# Number of option-order permutations to generate. 1 = no shuffling.",
    "# The current generator supports up to one version per answer option.",
    "versions: 4",
    "",
    "options: [A, B, C, D, E]",
    "",
    "id:",
    "  label: zID",
    "  prefix: z",
    "  digits: 7",
    "",
    "layout:",
    "  columns: 2",
    "",
    "# Optional: overrides the default instruction paragraph on the sheet.",
    "# instructions: \"Use a black or blue pen. ...\"",
    "",
    "sections:",
    "  - id: A",
    "    title: \"Section A\"",
    "    questions: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]",
    "    marks_each: 1",
    "",
    "  - id: B",
    "    title: \"Section B\"",
    "    questions: [11, 12, 13, 14, 15]",
    "    marks_each: 1",
    "    essays:",
    "      - number: 16",
    "        marks: 5"
  )
}

default_questions_md <- function() {
  c(
    "# COURSE1010 Exam Questions",
    "",
    "**Question 1 [1 mark]:** Replace this with the question text.",
    "",
    "a. First option",
    "b. Second option",
    "c. Third option",
    "d. Fourth option",
    "e. Fifth option",
    "",
    "<!-- Answer A -- explanation or notes here; hidden from rendered PDF if your template filters comments. -->",
    "",
    "**Question 2 [1 mark]:** Add more questions using the same pattern.",
    "",
    "a. First option",
    "b. Second option",
    "c. Third option",
    "d. Fourth option",
    "e. Fifth option",
    "",
    "<!-- Answer B -->"
  )
}

default_makefile <- function() {
  c(
    "OUTDIR ?= output",
    "CONFIG ?= exam.yml",
    "QUESTIONS ?= questions.md",
    "",
    ".PHONY: check versions sheets calibrate paper forms build",
    "",
    "check:",
    "\tbubblequiz check --config $(CONFIG)",
    "",
    "versions:",
    "\tbubblequiz versions --config $(CONFIG) --questions $(QUESTIONS) --outdir $(OUTDIR)",
    "",
    "sheets:",
    "\tbubblequiz sheets --config $(CONFIG) --outdir $(OUTDIR)",
    "",
    "calibrate:",
    "\tbubblequiz calibrate --config $(CONFIG) --outdir $(OUTDIR)",
    "",
    "paper:",
    "\tbubblequiz paper --config $(CONFIG) --outdir $(OUTDIR)",
    "",
    "forms:",
    "\tbubblequiz forms --config $(CONFIG) --outdir $(OUTDIR)",
    "",
    "build:",
    "\tbubblequiz build --config $(CONFIG) --questions $(QUESTIONS) --outdir $(OUTDIR)"
  )
}

write_scaffold_file <- function(path, lines) {
  if (file.exists(path)) {
    message("Exists, leaving unchanged: ", path)
    return(FALSE)
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(lines, path)
  message("Wrote: ", path)
  TRUE
}

#' Scaffold a course repository
#'
#' Writes starter `exam.yml`, `questions.md` and `Makefile` files without
#' overwriting existing files.
#'
#' @param dir Target course directory.
#' @return Invisibly, the target directory.
#' @export
init_course <- function(dir = ".") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write_scaffold_file(file.path(dir, "exam.yml"), default_exam_yml())
  write_scaffold_file(file.path(dir, "questions.md"), default_questions_md())
  write_scaffold_file(file.path(dir, "Makefile"), default_makefile())
  invisible(normalizePath(dir, mustWork = TRUE))
}
