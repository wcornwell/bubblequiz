# export.R -- from scored scan folders to a gradebook upload, with a review
# file that a person works through.
#
# A class is usually scanned in several batches, each its own scan folder with
# its own results.csv. The review file is one per quiz: it lists every sheet
# that needs a person, and it is where that person records what they decided.
# The pipeline reads those decisions back on every run and never discards them.

# Columns the pipeline writes into the review file (refreshed on every run),
# and the columns the reviewer fills in (never touched by the pipeline).
REVIEW_GENERATED <- c("scan", "files", "zid", "version", "answers", "provisional",
                      "reason", "status")
REVIEW_DECISIONS <- c("correct_zid", "correct_answers", "resolved", "comment")

#' Write a Moodle gradebook import file from scored scan folders
#'
#' Combines `results.csv` from each scan folder (run [score_results()] first,
#' or use [mark_quiz()] to do everything), and writes two files:
#'
#' * `output`: the upload, with a `Username` column (the zID) and one grade
#'   column named `grade_item`. In Moodle: Grades > Import > CSV file; map
#'   `Username` to "username" and the grade column to the grade item.
#' * `review`: every sheet that needed a person, open or resolved.
#'
#' A sheet goes in the upload only if it needs no review and was scored. The
#' upload is not written at all if two uploadable sheets carry the same zID,
#' since one of them must have been read or entered wrongly.
#'
#' @section The review file:
#' Each row is one flagged sheet: its scan folder and page files, what the
#' marker read (`zid`, `answers`: one letter per question, `*` = uncertain,
#' `-` = unanswered), the provisional score, the `reason`, and `status`
#' (`open`, `resolved` or `excluded`). Record decisions in four columns:
#'
#' * `correct_zid`: the right zID, if the one read is wrong or incomplete.
#' * `correct_answers`: only the answers that change, e.g. `Q5=A` or
#'   `Q2=B; Q5=-` (`-` for unanswered).
#' * `resolved`: `yes` once the sheet is checked. Needed when nothing changes
#'   (e.g. a sheet scanned back side first); implied by any correction.
#'   `exclude` leaves the sheet out of the upload for good -- for a sheet
#'   that was rescanned (the rescan is marked in its own right) or spoiled.
#' * `comment`: free text, kept as written.
#'
#' Then run [mark_quiz()] (or [score_results()] with `review =`, then
#' `export_moodle()`) again. Rows are never dropped and decision columns, or
#' any column you add, are never overwritten; a resolved sheet stays in the
#' file with `status = resolved`, so the record of what was decided is kept.
#'
#' @param dirs Scan folders, each holding a `results.csv`.
#' @param output Path for the upload CSV.
#' @param grade_item Name of the grade column, matching the Moodle grade item.
#' @param review Path for the review file.
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
  zid_pattern <- zid_regex(cfg)

  paths <- file.path(dirs, "results.csv")
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("results.csv not found in: ", paste(dirname(missing), collapse = ", "),
         "\nRun score_results() on each scan folder first.", call. = FALSE)
  }
  all <- do.call(rbind, lapply(paths, function(p) {
    r <- utils::read.csv(p, colClasses = "character")
    if (!all(c("needs_review", "answers") %in% names(r))) {
      stop(p, " was written by an older bubblequiz; re-run score_results().", call. = FALSE)
    }
    r$scan <- basename(dirname(p))
    r
  }))
  all$needs_review <- as.logical(all$needs_review)
  all$reviewed <- as.logical(all$reviewed)
  all$excluded <- as.logical(all$excluded %||% FALSE) %in% TRUE
  all$zid <- tolower(trimws(all$zid))
  value <- suppressWarnings(as.numeric(all[[grade]]))

  ok <- !all$needs_review & !all$excluded & !is.na(value) & grepl(zid_pattern, all$zid)
  why <- ifelse(all$needs_review, all$notes,
         ifelse(is.na(value), "not scored",
         ifelse(!grepl(zid_pattern, all$zid), "zID not valid", "")))

  dup <- unique(all$zid[ok][duplicated(all$zid[ok])])
  if (length(dup)) {
    rows <- all[ok & all$zid %in% dup, c("scan", "files", "zid")]
    stop("The same zID is on more than one sheet: ", paste(dup, collapse = ", "),
         "\n", paste(utils::capture.output(print(rows, row.names = FALSE)), collapse = "\n"),
         "\nCorrect the zID on the wrong sheet in the review file (correct_zid), ",
         "and run again.", call. = FALSE)
  }

  upload <- data.frame(Username = all$zid[ok], grade = value[ok], stringsAsFactors = FALSE)
  names(upload)[2] <- grade_item
  upload <- upload[order(upload$Username), , drop = FALSE]

  # Every sheet that needs a person now, or that a person has dealt with.
  listed <- !ok | all$reviewed
  current <- data.frame(
    scan = all$scan, files = all$files, zid = all$zid, version = all$exam_version,
    answers = all$answers, provisional = value,
    reason = ifelse(ok, all$notes, why),
    status = ifelse(all$excluded, "excluded", ifelse(ok, "resolved", "open")),
    stringsAsFactors = FALSE
  )[listed, , drop = FALSE]
  held <- merge_review(current, review, known = review_key(all$scan, all$files))

  # A decision in the file that the scores do not reflect means scoring was not
  # re-run with the review file since it was edited.
  decided <- Reduce(`|`, lapply(REVIEW_DECISIONS[1:3], function(col) {
    !is.na(held[[col]]) & nzchar(trimws(held[[col]]))
  }))
  applied <- review_key(held$scan, held$files) %in%
    review_key(all$scan, all$files)[all$reviewed]
  if (any(decided & !applied)) {
    warning(sum(decided & !applied), " decision(s) in the review file are not in the ",
            "scores yet. Run mark_quiz(), or score_results(review = ...) on each ",
            "folder, then export again.", call. = FALSE)
  }

  utils::write.csv(upload, output, row.names = FALSE)
  protect_review(review)
  utils::write.csv(held, review, row.names = FALSE, na = "")
  file.copy(review, review_snapshot(review), overwrite = TRUE)
  n_open <- sum(held$status == "open")
  message(sprintf("Upload:     %d student(s) -> %s", nrow(upload), output))
  message(sprintf("Review:     %d open, %d resolved -> %s", n_open,
                  sum(held$status == "resolved"), review))
  if (n_open) {
    message("Record decisions in the review file (correct_zid, correct_answers, ",
            "resolved, comment) and run again.")
  }
  invisible(list(upload = upload, review = held))
}

# The review file is edited by hand -- often in a spreadsheet that was opened
# before the pipeline last rewrote it, and saved over the newer version. So:
# every run keeps a dated copy of the file as it found it, and warns if
# decisions that were in the file the pipeline last wrote have since vanished.
review_history_dir <- function(review) file.path(dirname(review), ".review_history")
review_snapshot <- function(review) {
  dir.create(review_history_dir(review), showWarnings = FALSE)
  file.path(review_history_dir(review), paste0(basename(review), ".last-written"))
}

protect_review <- function(review) {
  if (!file.exists(review)) return(invisible())
  dir.create(review_history_dir(review), showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  dest <- function(k) file.path(review_history_dir(review), sub(
    "[.]csv$", paste0("_", stamp, if (k > 1) paste0("-", k), ".csv"), basename(review)))
  k <- 1L
  while (file.exists(dest(k))) k <- k + 1L
  file.copy(review, dest(k))
  lost <- lost_decisions(review)
  if (nrow(lost)) {
    warning(nrow(lost), " decision(s) that were in the review file last time are now empty ",
            "-- was it saved from a copy opened before the last run? e.g. ",
            paste(utils::head(sprintf("%s %s: %s = '%s'", lost$scan, lost$files, lost$column,
                                      lost$was), 3), collapse = "; "),
            ". The version last written is ", review_snapshot(review),
            "; earlier ones are in ", review_history_dir(review), ".", call. = FALSE)
  }
  invisible()
}

# lost_decisions: cells the reviewer had filled in the version the pipeline
# last wrote that are empty in the file now.
lost_decisions <- function(review) {
  none <- data.frame(scan = character(), files = character(), column = character(),
                     was = character(), stringsAsFactors = FALSE)
  snap <- review_snapshot(review)
  if (!file.exists(snap) || !file.exists(review)) return(none)
  was <- utils::read.csv(snap, colClasses = "character", check.names = FALSE)
  now <- utils::read.csv(review, colClasses = "character", check.names = FALSE)
  cols <- intersect(setdiff(names(was), REVIEW_GENERATED), names(now))
  m <- match(review_key(was$scan, was$files), review_key(now$scan, now$files))
  out <- list()
  for (col in cols) {
    filled_before <- !is.na(was[[col]]) & nzchar(trimws(was[[col]]))
    empty_now <- !is.na(m) & (is.na(now[[col]][m]) | !nzchar(trimws(now[[col]][m])))
    k <- which(filled_before & empty_now)
    if (length(k)) out[[col]] <- data.frame(scan = was$scan[k], files = was$files[k],
                                            column = col, was = was[[col]][k],
                                            stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out) else none
}

# merge_review: combine this run's review rows with the review file on disk.
# The generated columns come from this run; the reviewer's columns -- and any
# column the reviewer added -- come from disk and are never overwritten. A row
# on disk whose sheet is no longer flagged, or has disappeared from the scans
# altogether, is kept with its decisions and a status saying which.
merge_review <- function(current, review, known = character()) {
  for (col in REVIEW_DECISIONS) current[[col]] <- NA_character_
  if (!file.exists(review)) return(current[, c(REVIEW_GENERATED, REVIEW_DECISIONS)])

  old <- utils::read.csv(review, colClasses = "character", check.names = FALSE)
  if (!all(c("scan", "files") %in% names(old))) {
    stop(review, " is not a review file (no scan/files columns). Move it aside ",
         "and run again.", call. = FALSE)
  }
  key_cur <- review_key(current$scan, current$files)
  key_old <- review_key(old$scan, old$files)
  keep_cols <- setdiff(names(old), REVIEW_GENERATED)   # decisions + reviewer's own

  out <- current
  for (col in keep_cols) {
    if (!col %in% names(out)) out[[col]] <- NA_character_
    m <- match(key_cur, key_old)
    out[[col]][!is.na(m)] <- old[[col]][m[!is.na(m)]]
  }
  gone <- old[!key_old %in% key_cur, , drop = FALSE]
  if (nrow(gone)) {
    gone$status <- ifelse(review_key(gone$scan, gone$files) %in% known,
                          "no longer flagged", "not in current scans")
    for (col in setdiff(names(out), names(gone))) gone[[col]] <- NA_character_
    out <- rbind(out, gone[, names(out), drop = FALSE])
  }
  front <- c(REVIEW_GENERATED, REVIEW_DECISIONS)
  out[, c(front, setdiff(names(out), front)), drop = FALSE]
}

review_key <- function(scan, files) {
  paste(scan, vapply(strsplit(as.character(files), ";", fixed = TRUE),
                     function(f) paste(sort(basename(f)), collapse = ";"), character(1)))
}

zid_regex <- function(cfg) sprintf("^%s[0-9]{%d}$", cfg$id$prefix, as.integer(cfg$id$digits))

# review_decisions: turn the reviewer's columns for one scan folder into
# override rows (file, page, zid, name, question, response), the form
# score_results() already applies. A malformed entry stops the run with the
# row named, rather than being skipped.
review_decisions <- function(review, scan, cfg) {
  empty <- data.frame(file = character(), page = character(), zid = character(),
                      name = character(), question = character(), response = character(),
                      stringsAsFactors = FALSE)
  r <- utils::read.csv(review, colClasses = "character", check.names = FALSE)
  if (!nrow(r) || !"scan" %in% names(r)) return(empty)
  for (col in REVIEW_DECISIONS) if (!col %in% names(r)) r[[col]] <- NA_character_
  blank <- function(x) is.na(x) | !nzchar(trimws(x))
  rows <- which(r$scan == scan)
  out <- list()
  add <- function(file, question, response) {
    out[[length(out) + 1L]] <<- data.frame(file = file, page = NA_character_,
                                           zid = NA_character_, name = NA_character_,
                                           question = question, response = response,
                                           stringsAsFactors = FALSE)
  }
  for (i in rows) {
    file <- strsplit(r$files[i], ";", fixed = TRUE)[[1]][1]
    where <- sprintf("review file row %d (%s)", i + 1L, r$files[i])
    decided <- FALSE
    if (!blank(r$correct_zid[i])) {
      z <- tolower(trimws(r$correct_zid[i]))
      if (!grepl("^[a-z]", z)) z <- paste0(cfg$id$prefix, z)
      if (!grepl(zid_regex(cfg), z)) {
        stop(where, ": correct_zid '", r$correct_zid[i], "' is not a valid ",
             cfg$id$label, call. = FALSE)
      }
      add(file, "zid", z)
      decided <- TRUE
    }
    if (!blank(r$correct_answers[i])) {
      for (tok in strsplit(trimws(r$correct_answers[i]), "[;,[:space:]]+")[[1]]) {
        m <- regmatches(tok, regexec("^[Qq]?([0-9]+)=([A-Za-z-]?)$", tok))[[1]]
        if (length(m) != 3) {
          stop(where, ": can't read '", tok, "' in correct_answers; write e.g. Q5=A, ",
               "or Q5=- for unanswered", call. = FALSE)
        }
        q <- as.integer(m[2]); a <- toupper(m[3])
        if (!q %in% cfg$questions) stop(where, ": there is no question ", q, call. = FALSE)
        if (a == "-") a <- ""
        if (nzchar(a) && !a %in% cfg$options) {
          stop(where, ": '", m[3], "' is not an option for Q", q, call. = FALSE)
        }
        add(file, as.character(q), a)
      }
      decided <- TRUE
    }
    res <- tolower(trimws(r$resolved[i]))
    if (!blank(r$resolved[i]) && res %in% c("exclude", "excluded", "rescanned", "void")) {
      add(file, "exclude", NA_character_)
      next
    }
    if (!blank(r$resolved[i]) && res %in% c("yes", "y", "true", "1", "x", "ok", "done")) {
      decided <- TRUE
    } else if (!blank(r$resolved[i]) && !res %in% c("no", "n", "false", "0")) {
      stop(where, ": resolved should be yes, no or exclude, not '", r$resolved[i], "'",
           call. = FALSE)
    }
    if (decided) add(file, "ok", NA_character_)
  }
  if (length(out)) do.call(rbind, out) else empty
}

#' Mark a whole quiz: every scan in a folder, through to the Moodle upload
#'
#' Runs the complete pipeline on every PDF in `folder` -- render, sequence
#' check, marking, sheet assembly -- then scores each scan folder with the
#' decisions in the review file applied, and writes the upload and the review
#' file. Scans already marked are not marked again unless the PDF has changed
#' (or `remark = TRUE`), so running it again after reviewing takes seconds.
#'
#' For each quiz: put the scan PDFs in one folder, run `mark_quiz()`, work
#' through the review file, run `mark_quiz()` again, upload.
#'
#' @param folder Folder holding the scan PDFs for one quiz.
#' @param config Path to the exam config YAML.
#' @param forms Folder holding `quizform_v<N>.pdf` for every version.
#' @param key Path to `answer_key.csv`.
#' @param grade_item Name of the Moodle grade item.
#' @param output Path for the upload CSV.
#' @param review Path for the review file.
#' @param remark Mark every scan again even if already marked.
#' @param accept_review Run even though decisions that were in the review
#'   file after the last run have since been emptied. Without it, that stops
#'   the run: it usually means the file was saved from a stale copy.
#' @return Invisibly, what [export_moodle()] returns.
#' @export
mark_quiz <- function(folder,
                      config     = "exam.yml",
                      forms      = "output",
                      key        = file.path(forms, "answer_key.csv"),
                      grade_item = "Quiz",
                      output     = file.path(folder, "moodle_import.csv"),
                      review     = file.path(folder, "review.csv"),
                      remark     = FALSE,
                      accept_review = FALSE) {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  pdfs <- sort(list.files(folder, pattern = "[.]pdf$", full.names = TRUE, ignore.case = TRUE))
  lost <- if (accept_review) data.frame() else lost_decisions(review)
  if (nrow(lost)) {
    stop(nrow(lost), " decision(s) that were in ", basename(review), " after the last run are ",
         "now empty -- it looks like it was saved from a copy opened before that run:\n",
         paste(sprintf("  %s %s: %s was '%s'", lost$scan, lost$files, lost$column, lost$was),
               collapse = "\n"),
         "\nThe version the last run wrote is ", review_snapshot(review), ". Put the ",
         "decisions back (or copy your new edits into that version), then run again. ",
         "To accept the file as it is, pass accept_review = TRUE.", call. = FALSE)
  }
  if (!length(pdfs)) stop("No scan PDFs in ", folder, call. = FALSE)
  dirs <- file.path(dirname(pdfs), tools::file_path_sans_ext(basename(pdfs)))

  for (k in seq_along(pdfs)) {
    pdf <- pdfs[k]; d <- dirs[k]
    sheets <- file.path(d, "sheets.csv")
    fresh <- file.exists(sheets) && file.mtime(sheets) > file.mtime(pdf)
    if (fresh && !remark) {
      message("Already marked: ", basename(pdf))
      next
    }
    message("Marking: ", basename(pdf))
    preprocess_scans(pdf, cfg, force = TRUE)
    check_scan_sequence(d)
    mark_scans_cv(d, config = cfg, layout = file.path(forms, "layout.R"), forms = forms)
    aggregate_sheets(d, config = cfg)
  }
  for (d in dirs) {
    utils::capture.output(score_results(d, key = key, config = cfg, output = NULL,
                                        review = review))
  }
  export_moodle(dirs, output = output, grade_item = grade_item, review = review,
                config = cfg)
}
