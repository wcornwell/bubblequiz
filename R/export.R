# export.R -- turn scored scan folders into a gradebook upload.
#
# A class is usually scanned in several batches, each its own scan folder with
# its own results.csv. This combines them, keeps only sheets that are safe to
# upload, and lists everything else for a person to deal with.

#' Write a Moodle gradebook import file from scored scan folders
#'
#' Combines `results.csv` from each scan folder (run [score_results()] first),
#' and writes two files:
#'
#' * `output`: the upload, with a `Username` column (the zID) and one grade
#'   column named `grade_item`. In Moodle: Grades > Import > CSV file; map
#'   `Username` to "username" and the grade column to the grade item.
#' * `review`: every sheet left out of the upload, with its scan folder, page
#'   files, provisional score and the reason.
#'
#' A sheet goes in the upload only if it needs no review and was scored. The
#' upload is not written at all if two uploadable sheets carry the same zID,
#' since one of them must have been read or entered wrongly.
#'
#' @param dirs Scan folders, each holding a `results.csv`.
#' @param output Path for the upload CSV.
#' @param grade_item Name of the grade column, matching the Moodle grade item.
#' @param review Path for the list of sheets left out.
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param grade `"score"` (questions correct) or `"marks"` (weighted marks).
#' @return Invisibly, a list with the `upload` and `review` data frames.
#' @export
export_moodle <- function(dirs,
                          output     = "moodle_import.csv",
                          grade_item = "Quiz",
                          review     = sub("[.]csv$", "_to_review.csv", output),
                          config     = default_config_path(),
                          grade      = c("score", "marks")) {
  grade <- match.arg(grade)
  cfg <- if (is.list(config)) config else load_exam_config(config)
  zid_pattern <- sprintf("^%s[0-9]{%d}$", cfg$id$prefix, as.integer(cfg$id$digits))

  paths <- file.path(dirs, "results.csv")
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("results.csv not found in: ", paste(dirname(missing), collapse = ", "),
         "\nRun score_results() on each scan folder first.", call. = FALSE)
  }
  all <- do.call(rbind, lapply(paths, function(p) {
    r <- utils::read.csv(p, colClasses = "character")
    if (!("needs_review" %in% names(r))) {
      stop(p, " has no needs_review column; re-run score_results() with this ",
           "version of bubblequiz.", call. = FALSE)
    }
    r$scan <- basename(dirname(p))
    r
  }))
  all$needs_review <- as.logical(all$needs_review)
  all$zid <- tolower(trimws(all$zid))
  value <- suppressWarnings(as.numeric(all[[grade]]))

  ok <- !all$needs_review & !is.na(value) & grepl(zid_pattern, all$zid)
  why <- ifelse(all$needs_review, all$notes,
         ifelse(is.na(value), "not scored",
         ifelse(!grepl(zid_pattern, all$zid), "zID not valid", "")))

  dup <- unique(all$zid[ok][duplicated(all$zid[ok])])
  if (length(dup)) {
    rows <- all[ok & all$zid %in% dup, c("scan", "file", "zid")]
    stop("The same zID is on more than one sheet: ", paste(dup, collapse = ", "),
         "\n", paste(utils::capture.output(print(rows, row.names = FALSE)), collapse = "\n"),
         "\nCorrect the zID on the wrong sheet with an override, re-score, and export again.",
         call. = FALSE)
  }

  upload <- data.frame(Username = all$zid[ok], grade = value[ok], stringsAsFactors = FALSE)
  names(upload)[2] <- grade_item
  upload <- upload[order(upload$Username), , drop = FALSE]

  files_col <- if ("files" %in% names(all)) all$files else all$file
  held <- data.frame(scan = all$scan, files = files_col %||% all$file, zid = all$zid,
                     version = all$exam_version, provisional = value, reason = why,
                     stringsAsFactors = FALSE)[!ok, , drop = FALSE]

  utils::write.csv(upload, output, row.names = FALSE)
  utils::write.csv(held, review, row.names = FALSE)
  message(sprintf("Upload:     %d student(s) -> %s", nrow(upload), output))
  message(sprintf("For review: %d sheet(s)   -> %s", nrow(held), review))
  if (nrow(held)) {
    message("Resolve these with overrides.csv in their scan folder, re-run ",
            "score_results(), then export again.")
  }
  invisible(list(upload = upload, review = held))
}
