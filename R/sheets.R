# sheets.R -- combine the pages of a multi-page form into one row per student.
#
# The marker works a page at a time: each scanned page is read on its own, and
# only the front of a sheet carries a zID grid. For a form that runs to more
# than one page that leaves the answers from the back with nobody attached to
# them. This joins the pages of each sheet back together, using the sheet
# grouping that check_scan_sequence() derived from the per-page QR codes.

#' Combine scanned pages into one row per student
#'
#' Groups the rows of `progress.csv` by the `sheet` column written by
#' [check_scan_sequence()], and reduces each sheet to a single record: the zID
#' and version from the front page, and every question answered from whichever
#' page carries its bubble row.
#'
#' A single-page form needs no aggregation; in that case each page is its own
#' sheet and the output is a copy of the input.
#'
#' @param dir Folder created by [preprocess_scans()] and marked by
#'   [mark_scans_cv()].
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param output CSV path for the aggregated sheets; defaults to `sheets.csv`
#'   inside `dir`.
#' @return Invisibly, the aggregated data frame.
#' @export
aggregate_sheets <- function(dir,
                             config = default_config_path(),
                             output = NULL) {
  if (!dir.exists(dir)) stop("Directory not found: ", dir, call. = FALSE)
  cfg <- if (is.list(config)) config else load_exam_config(config)
  if (is.null(output)) output <- file.path(dir, "sheets.csv")

  csv_path <- file.path(dir, "progress.csv")
  if (!file.exists(csv_path)) {
    stop("progress.csv not found in ", dir,
         " -- run `bubblequiz preprocess` first.", call. = FALSE)
  }
  progress <- readr::read_csv(csv_path, show_col_types = FALSE)

  if (!("sheet" %in% names(progress)) || all(is.na(progress$sheet))) {
    message("No sheet grouping found; treating every page as its own sheet.")
    progress$sheet <- seq_len(nrow(progress))
    progress$sheet_page <- 1L
  }

  q_cols <- paste0("q", cfg$questions)
  missing_q <- setdiff(q_cols, names(progress))
  if (length(missing_q)) {
    stop("progress.csv is missing question column(s): ",
         paste(missing_q, collapse = ", "), call. = FALSE)
  }

  first_non_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else x[[1]]
  }

  unattributed <- sum(is.na(progress$sheet))
  grouped <- progress[!is.na(progress$sheet), , drop = FALSE]
  sheets <- sort(unique(grouped$sheet))

  # How many pages a complete sheet has. Taken from the stack itself rather
  # than assumed, so a sheet that is short a page is caught instead of being
  # scored as though the missing questions were left blank.
  expected_pages <- suppressWarnings(max(as.integer(grouped$sheet_page), na.rm = TRUE))
  if (!is.finite(expected_pages)) expected_pages <- 1L

  rows <- lapply(sheets, function(sh) {
    pg <- grouped[grouped$sheet == sh, , drop = FALSE]
    pg <- pg[order(pg$sheet_page), , drop = FALSE]

    notes <- character(0)

    # Version must be consistent across the pages of one sheet. The sequence
    # check already rejects a mixed sheet, so disagreement here means a page
    # was misread rather than misfed.
    vs <- unique(stats::na.omit(as.character(pg$exam_version)))
    if (length(vs) > 1) {
      notes <- c(notes, paste0("version disagreement across pages (",
                               paste(vs, collapse = "/"), ")"))
    }

    zids <- unique(stats::na.omit(as.character(pg$zid)))
    if (length(zids) == 0) notes <- c(notes, "no zID read on this sheet")
    if (length(zids) > 1) {
      notes <- c(notes, paste0("conflicting zIDs (", paste(zids, collapse = "/"), ")"))
    }

    if (any(pg$status == "error", na.rm = TRUE)) notes <- c(notes, "a page errored")

    if (nrow(pg) < expected_pages) {
      notes <- c(notes, sprintf("incomplete sheet: %d of %d pages",
                                nrow(pg), expected_pages))
    }

    page_notes <- stats::na.omit(as.character(pg$notes))
    page_notes <- page_notes[nzchar(page_notes)]

    row <- data.frame(
      sheet        = sh,
      pages        = nrow(pg),
      files        = paste(basename(as.character(pg$file)), collapse = ";"),
      page         = pg$page[1],
      file         = as.character(pg$file[1]),
      status       = if (all(pg$status == "done", na.rm = TRUE)) "done" else "error",
      zid          = if (length(zids) >= 1) zids[1] else NA_character_,
      name         = first_non_na(as.character(pg$name)),
      exam_version = if (length(vs) >= 1) vs[1] else NA_character_,
      confidence   = first_non_na(as.character(pg$confidence)),
      needs_review = any(as.logical(pg$needs_review), na.rm = TRUE) || length(notes) > 0,
      notes        = paste(c(notes, page_notes), collapse = " | "),
      error        = first_non_na(as.character(pg$error)),
      stringsAsFactors = FALSE
    )
    # Each question is answered on exactly one page; the other pages hold NA.
    for (qc in q_cols) row[[qc]] <- first_non_na(as.character(pg[[qc]]))
    row
  })

  out <- do.call(rbind, rows)
  utils::write.csv(out, output, row.names = FALSE)

  message("Sheets:       ", nrow(out))
  message("Needs review: ", sum(out$needs_review, na.rm = TRUE))
  if (unattributed > 0) {
    message("Unattributed pages (not in a complete sheet): ", unattributed)
    warning(unattributed, " page(s) could not be attributed to a sheet and are ",
            "excluded from ", basename(output), ". Check scan_sequence.csv.",
            call. = FALSE)
  }
  message("Wrote: ", output)
  invisible(out)
}
