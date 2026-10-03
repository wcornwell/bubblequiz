# config.R -- single source of truth for exam shape.
#
# Every other script in this repo starts with:
#     cfg <- load_exam_config()
# and reads the constants it needs from `cfg`, rather than defining its own.
# Edit exam.yml; never hard-code question numbers, version counts or layout.

#' Default exam config path
#'
#' Project paths always resolve against the current working directory, which is
#' the course repository -- the package itself never assumes it is the project.
#'
#' @return Path to the exam config: `$BUBBLEQUIZ_CONFIG` if set, else `exam.yml`.
#' @export
default_config_path <- function() {
  env <- Sys.getenv("BUBBLEQUIZ_CONFIG")
  if (nzchar(env)) env else "exam.yml"
}

#' Locate a file shipped inside the installed package
#'
#' @param ... Path components below `inst/`.
#' @return Absolute path to the installed file.
#' @export
bq_file <- function(...) {
  p <- system.file(..., package = "bubblequiz")
  if (!nzchar(p)) {
    stop("bubblequiz install is missing: ", file.path(...), call. = FALSE)
  }
  p
}

# ---------------------------------------------------------------------------
# Column naming
# ---------------------------------------------------------------------------
# Two-column sheets (the common case) use "left"/"right" so generated layout
# files read naturally; wider sheets fall back to col1..colN.
column_names <- function(n_cols) {
  if (n_cols == 2) c("left", "right") else paste0("col", seq_len(n_cols))
}

# ---------------------------------------------------------------------------
# Layout: assign every printed item (MCQ or essay label) to a row and column
# ---------------------------------------------------------------------------
# Within a section, items are sorted by question number, split into
# ceiling(n / n_cols) rows, and filled COLUMN-MAJOR: the first n_rows items go
# down the left column, the next n_rows down the right. This reproduces the
# hand-written tabulars of the original BEES2041 bubble sheet exactly.
layout_section <- function(items, n_cols) {
  items  <- items[order(items)]
  n      <- length(items)
  n_rows <- ceiling(n / n_cols)
  cols   <- column_names(n_cols)

  rows <- vector("list", n_rows)
  for (r in seq_len(n_rows)) rows[[r]] <- list()

  for (i in seq_len(n)) {
    col_idx <- ((i - 1L) %/% n_rows) + 1L
    row_idx <- ((i - 1L) %% n_rows) + 1L
    rows[[row_idx]][[cols[col_idx]]] <- items[i]
  }
  rows
}

# ---------------------------------------------------------------------------
# Validation -- fail loudly on the mistakes that silently corrupt a marking run
# ---------------------------------------------------------------------------
validate_exam_config <- function(cfg) {
  err <- function(...) stop("exam config: ", ..., call. = FALSE)

  if (length(cfg$sections) == 0) err("no sections defined")
  if (length(cfg$questions) == 0) err("no MCQ questions defined")

  dup <- cfg$questions[duplicated(cfg$questions)]
  if (length(dup) > 0) err("duplicate question number(s): ", paste(dup, collapse = ", "))

  both <- intersect(cfg$questions, cfg$essay_questions)
  if (length(both) > 0) {
    err("question(s) listed as both MCQ and essay: ", paste(both, collapse = ", "))
  }

  dup_e <- cfg$essay_questions[duplicated(cfg$essay_questions)]
  if (length(dup_e) > 0) err("duplicate essay number(s): ", paste(dup_e, collapse = ", "))

  if (length(cfg$options) < 2) err("need at least 2 answer options")
  if (cfg$n_versions < 1) err("versions must be >= 1")
  if (cfg$n_versions > length(cfg$options)) {
    err("versions must be <= number of answer options (", length(cfg$options), ")")
  }

  if (cfg$id$digits < 1 || cfg$id$digits > 12) err("id.digits must be between 1 and 12")
  if (cfg$n_cols < 1 || cfg$n_cols > 4) err("layout.columns must be between 1 and 4")

  invisible(TRUE)
}

# Cross-check the calibrator's findings against the config. Called by
# calibrate_coords.R once the bubble rows have been detected in the PDF.
validate_layout_match <- function(cfg, n_rows_found, n_x_groups_found) {
  expected_rows <- length(cfg$bubble_rows)
  if (n_rows_found != expected_rows) {
    stop(sprintf(
      "Calibration mismatch: config expects %d bubble rows, found %d in the PDF.\nRebuild the sheet (make sheets) before calibrating.",
      expected_rows, n_rows_found), call. = FALSE)
  }
  expected_x <- length(cfg$options) * cfg$n_cols
  if (n_x_groups_found != expected_x) {
    stop(sprintf(
      "Calibration mismatch: config expects %d bubble x-positions (%d options x %d columns), found %d.",
      expected_x, length(cfg$options), cfg$n_cols, n_x_groups_found), call. = FALSE)
  }
  invisible(TRUE)
}

# Answer key must carry one column per version.
validate_answer_key <- function(cfg, key) {
  want <- paste0("answer_v", cfg$valid_versions)
  miss <- setdiff(want, names(key))
  if (length(miss) > 0) {
    stop("Answer key is missing column(s): ", paste(miss, collapse = ", "),
         "\nRe-run `make versions` after changing `versions:` in the config.",
         call. = FALSE)
  }
  key_qs <- sort(as.integer(key$question))
  if (!identical(key_qs, as.integer(cfg$questions))) {
    stop("Answer key questions (", paste(key_qs, collapse = ","),
         ") do not match the config (", paste(cfg$questions, collapse = ","), ").",
         call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# load_exam_config
# ---------------------------------------------------------------------------

#' Load and validate an exam configuration
#'
#' Reads the YAML exam config and derives everything downstream needs: the MCQ
#' question list (essay questions excluded), the version labels, the row/column
#' layout of the printed sheet, and the vision prompt. Every other function in
#' the package takes the result of this rather than hard-coding exam shape.
#'
#' @param path Path to the exam config YAML, relative to the working directory.
#' @return A list describing the exam; see `vignette`-free docs in README.
#' @export
load_exam_config <- function(path = default_config_path()) {
  if (!file.exists(path)) {
    stop("Exam config not found: ", path,
         "\nRun from the repo root, or set BUBBLEQUIZ_CONFIG.", call. = FALSE)
  }
  y <- yaml::read_yaml(path)

  cfg <- list(
    path         = normalizePath(path),
    course       = y$course       %||% "",
    title        = y$title        %||% y$course %||% "Exam",
    date         = y$date         %||% "",
    subtitle     = y$subtitle     %||% "Multiple Choice Answer Sheet",
    duration     = y$duration     %||% "",
    total_marks  = y$total_marks  %||% NA,
    n_versions   = as.integer(y$versions %||% 1L),
    options      = toupper(as.character(y$options %||% c("A", "B", "C", "D", "E"))),
    id           = list(
      label  = y$id$label  %||% "zID",
      prefix = y$id$prefix %||% "z",
      digits = as.integer(y$id$digits %||% 7L)
    ),
    n_cols       = as.integer(y$layout$columns %||% 2L),
    # Vertical breathing room on inline quiz forms. All are LaTeX lengths.
    # Raising question_spacing is the usual way to spread fewer questions
    # across more pages so students have room to think and write.
    spacing      = list(
      question = as.character(y$layout$question_spacing %||% "5pt"),
      option   = as.character(y$layout$option_spacing   %||% "1pt"),
      stem     = as.character(y$layout$stem_spacing     %||% "4pt")
    ),
    instructions = y$instructions %||% NULL,
    sections     = y$sections
  )

  # Normalise each section: integer question vector + essay tibble-ish list.
  cfg$sections <- lapply(cfg$sections, function(s) {
    s$questions <- if (is.null(s$questions)) integer(0) else as.integer(s$questions)
    s$essays    <- if (is.null(s$essays)) list() else s$essays
    s$essays    <- lapply(s$essays, function(e) {
      list(number = as.integer(e$number), marks = e$marks %||% NA)
    })
    s$marks_each <- s$marks_each %||% 1
    s
  })

  cfg$questions <- sort(unlist(lapply(cfg$sections, `[[`, "questions")))
  cfg$essay_questions <- sort(unlist(lapply(cfg$sections, function(s) {
    vapply(s$essays, `[[`, integer(1), "number")
  })))
  if (is.null(cfg$essay_questions)) cfg$essay_questions <- integer(0)

  cfg$valid_versions <- as.character(seq_len(cfg$n_versions))
  cfg$col_names      <- column_names(cfg$n_cols)

  validate_exam_config(cfg)

  # Rows across the whole sheet, in print order, plus derived lookup tables.
  rows <- list()
  question_col <- character(0)
  for (s in cfg$sections) {
    items <- c(s$questions, vapply(s$essays, `[[`, integer(1), "number"))
    if (length(items) == 0) next
    s_rows <- layout_section(items, cfg$n_cols)
    for (r in s_rows) {
      # A row is a named list: column name -> question number.
      rows[[length(rows) + 1L]] <- r
      for (col in names(r)) {
        q <- r[[col]]
        if (q %in% cfg$questions) question_col[as.character(q)] <- col
      }
    }
  }
  cfg$rows         <- rows
  cfg$question_col <- question_col[as.character(cfg$questions)]

  # Rows that actually contain bubbles (essay-only rows have no A-E letters and
  # so are invisible to the calibrator).
  cfg$bubble_rows <- Filter(function(r) {
    any(vapply(r, function(q) q %in% cfg$questions, logical(1)))
  }, rows)

  cfg
}

`%||%` <- function(a, b) if (is.null(a)) b else a
