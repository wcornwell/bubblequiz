# Shared fixtures: a minimal four-question config, and a progress table shaped
# the way the marker writes one for a two-page form.

test_config <- function(questions = 1:4) {
  path <- tempfile(fileext = ".yml")
  writeLines(c(
    'course: TEST', 'title: "T"', 'date: "d"', 'subtitle: "s"', 'duration: "1"',
    sprintf('total_marks: %d', length(questions)),
    'versions: 2', 'options: [A, B, C, D, E]',
    'id:', '  label: zID', '  prefix: z', '  digits: 7',
    'layout:', '  columns: 1',
    'sections:', '  - id: A', '    title: "A"',
    sprintf('    questions: [%s]', paste(questions, collapse = ", ")),
    '    marks_each: 1'
  ), path)
  load_exam_config(path)
}

# Two students on two-page sheets: Q1-2 on the front, Q3-4 on the back. Only the
# front carries a zID, and each page holds NA for the other page's questions --
# exactly what mark_scans_cv() writes.
two_page_progress <- function() {
  data.frame(
    status = "done", api_call_ok = TRUE,
    file = paste0("pages/page_", sprintf("%04d", 1:4), ".png"),
    page = 1:4,
    zid = c("z1111111", NA, "z2222222", NA),
    name = NA_character_, needs_review = FALSE,
    exam_version = c("1", "1", "2", "2"),
    confidence = "high", notes = NA_character_, error = NA_character_,
    sheet = c(1L, 1L, 2L, 2L), sheet_page = c(1L, 2L, 1L, 2L),
    q1 = c("A", NA, "B", NA), q2 = c("B", NA, "C", NA),
    q3 = c(NA, "C", NA, "D"), q4 = c(NA, "D", NA, "E"),
    stringsAsFactors = FALSE
  )
}

# Write a progress table into a scratch scan folder and return the folder.
scan_dir_with <- function(progress) {
  dir <- file.path(tempfile("bq-scan-"))
  dir.create(dir, recursive = TRUE)
  utils::write.csv(progress, file.path(dir, "progress.csv"), row.names = FALSE)
  dir
}
