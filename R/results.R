# results.R -- score marked sheets against the answer key.
#
# Reads progress.csv (written by the marker), applies any manual corrections
# from overrides.csv, scores each student against their own exam version, and
# writes results.csv. Pages that never marked cleanly still get a row, with NA
# scores, so nobody silently disappears between the scanner and the gradebook.
#
# Every row carries needs_review and notes. A flagged sheet keeps its
# provisional score but stays flagged until a person has entered overrides for
# it (see score_results()), and export_moodle() leaves flagged sheets out of the
# upload.

#' Score marked bubble sheets against the answer key
#'
#' Corrections go in `overrides.csv` in the scan folder, one row per change,
#' with columns `file`, `page`, `zid`, `name`, `question`, `response`. Identify
#' the sheet by `file` (any page of it, e.g. `page_0036.png`) or by `zid`.
#' `question` is a question number (`response` = the letter, or blank for
#' unanswered), `zid` (`response` = the corrected zID), `ok` (checked, no
#' change needed), or `exclude` (leave the sheet out, e.g. it was rescanned). Any row for a sheet marks it reviewed; a sheet still needs
#' review while it holds an uncertain answer (`B*`) or an invalid zID.
#'
#' @param dir Folder created by [preprocess_scans()] and filled in by [mark_scans_cv()].
#' @param key Path to `answer_key.csv` from [generate_versions()].
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param output Output CSV path; defaults to `results.csv` inside `dir`.
#' @param review The quiz's review file (see [export_moodle()]); decisions in it
#'   for sheets in this scan folder are applied as overrides.
#' @return Invisibly, the results data frame.
#' @export
score_results <- function(dir,
                          key    = "output/answer_key.csv",
                          config = default_config_path(),
                          output = NULL,
                          review = NULL) {
  if (!dir.exists(dir)) stop("Directory not found: ", dir, call. = FALSE)
  cfg <- if (is.list(config)) config else load_exam_config(config)

  EXPECTED_QUESTIONS <- cfg$questions
  VALID_VERSIONS     <- cfg$valid_versions

  # Marks per question, taken from the section each question belongs to.
  marks_per_q <- stats::setNames(rep(1, length(cfg$questions)), as.character(cfg$questions))
  for (sec in cfg$sections) {
    for (q in sec$questions) marks_per_q[[as.character(q)]] <- as.numeric(sec$marks_each)
  }

  key_path <- key
  if (is.null(output)) output <- file.path(dir, "results.csv")

  csv_path       <- file.path(dir, "progress.csv")
  overrides_path <- file.path(dir, "overrides.csv")

  if (!file.exists(csv_path)) {
    stop("progress.csv not found in ", dir,
         " -- run `bubblequiz preprocess` first.", call. = FALSE)
  }
  if (!file.exists(key_path)) stop("Answer key not found: ", key_path, call. = FALSE)

  # ---------------------------------------------------------------------------
  # Read inputs
  # ---------------------------------------------------------------------------
  # A multi-page form is scored per sheet, not per page: sheets.csv carries one
  # row per student with the answers from every page already joined. It is
  # preferred whenever aggregate_sheets() has been run, and a single-page form
  # produces the same rows either way.
  sheets_path <- file.path(dir, "sheets.csv")
  if (file.exists(sheets_path)) {
    message("Reading sheets CSV:   ", sheets_path)
    progress <- readr::read_csv(sheets_path, show_col_types = FALSE)
  } else {
    message("Reading progress CSV: ", csv_path)
    progress <- readr::read_csv(csv_path, show_col_types = FALSE)
    n_form_pages <- length(unique(stats::na.omit(as.integer(progress$sheet_page))))
    if (n_form_pages > 1) {
      warning("progress.csv holds ", n_form_pages, " pages per sheet but sheets.csv ",
              "was not found. Run aggregate_sheets() first, or each page will be ",
              "scored as if it were a whole paper.", call. = FALSE)
    }
  }
  if (!("name" %in% names(progress))) {
    progress$name <- NA_character_
  }
  if (!("needs_review" %in% names(progress))) progress$needs_review <- FALSE
  if (!("notes" %in% names(progress))) progress$notes <- NA_character_
  progress$needs_review <- as.logical(progress$needs_review) %in% TRUE
  progress$reviewed <- FALSE
  progress$excluded <- FALSE
  zid_pattern <- sprintf("^%s[0-9]{%d}$", cfg$id$prefix, as.integer(cfg$id$digits))

  # All the page files of each row, so an override can name any page of a
  # sheet -- a reviewer checking an answer on the back names the back page.
  row_files <- if ("files" %in% names(progress)) {
    strsplit(as.character(progress$files), ";", fixed = TRUE)
  } else {
    as.list(as.character(progress$file))
  }


  overrides <- NULL
  if (file.exists(overrides_path)) {
    message("Reading overrides:    ", overrides_path)
    overrides <- utils::read.csv(overrides_path, colClasses = "character")
  }
  # Decisions entered in the quiz's review file (see export_moodle()) for the
  # sheets in this scan folder are applied exactly like overrides.csv rows.
  if (!is.null(review) && file.exists(review)) {
    from_review <- review_decisions(review, basename(normalizePath(dir)), cfg)
    if (nrow(from_review)) {
      message("Review decisions:     ", nrow(from_review), " from ", review)
      if (is.null(overrides)) {
        overrides <- from_review
      } else {
        for (col in setdiff(names(from_review), names(overrides))) {
          overrides[[col]] <- rep(NA_character_, nrow(overrides))
        }
        overrides <- rbind(overrides[, names(from_review), drop = FALSE], from_review)
      }
    }
  }

  if (!is.null(overrides) && nrow(overrides) > 0) {
    required_cols <- c("page", "zid", "name", "question", "response")
    has_override_file_col <- "file" %in% names(overrides)

    if (!all(required_cols %in% names(overrides))) {
      stop("Overrides file must contain columns: page, zid, name, question, response", call. = FALSE)
    }

    n_applied <- 0L
    n_skipped <- 0L

    for (i in seq_len(nrow(overrides))) {
      ov <- overrides[i, ]

      # `question` is a question number, or one of three keywords:
      #   zid     -- `response` is the corrected zID
      #   ok      -- the sheet was checked and needs no change
      #   exclude -- leave the sheet out entirely (e.g. rescanned, or spoiled)
      q_text <- tolower(trimws(as.character(ov$question)))
      q_raw <- suppressWarnings(as.integer(q_text))
      kind <- if (q_text %in% c("zid", "ok", "exclude")) q_text else "answer"
      if (kind == "answer" && (is.na(q_raw) || !q_raw %in% EXPECTED_QUESTIONS)) {
        warning(sprintf("Skipping override row %d: invalid question '%s'", i, ov$question))
        n_skipped <- n_skipped + 1L
        next
      }

      q_col <- if (kind == "answer") paste0("q", q_raw) else NA_character_
      file_key <- ""
      if (has_override_file_col) {
        file_val <- ov[["file"]]
        if (length(file_val) > 0 && !is.na(file_val)) {
          file_key <- trimws(as.character(file_val))
        }
      }

      zid_key <- ""
      zid_val <- ov[["zid"]]
      if (length(zid_val) > 0 && !is.na(zid_val)) {
        zid_key <- trimws(as.character(zid_val))
      }

      has_file <- nchar(file_key) > 0
      has_zid  <- nchar(zid_key) > 0

      if (!has_file && !has_zid) {
        warning(sprintf("Skipping override row %d: either file or zid must be provided", i))
        n_skipped <- n_skipped + 1L
        next
      }

      idx_file <- if (has_file) {
        which(vapply(row_files, function(f) basename(file_key) %in% basename(f), logical(1)))
      } else integer(0)
      idx_zid  <- if (has_zid)  which(as.character(progress$zid) == zid_key)  else integer(0)

      idx <- integer(0)
      if (has_file && has_zid) {
        if (length(idx_file) != 1) {
          warning(sprintf("Skipping override row %d: file '%s' matched %d rows", i, file_key, length(idx_file)))
          n_skipped <- n_skipped + 1L
          next
        }
        if (length(idx_zid) != 1) {
          warning(sprintf("Skipping override row %d: zid '%s' matched %d rows", i, zid_key, length(idx_zid)))
          n_skipped <- n_skipped + 1L
          next
        }
        if (idx_file[1] != idx_zid[1]) {
          warning(sprintf("Skipping override row %d: file '%s' and zid '%s' do not refer to the same record", i, file_key, zid_key))
          n_skipped <- n_skipped + 1L
          next
        }
        idx <- idx_file[1]
      } else if (has_file) {
        if (length(idx_file) != 1) {
          warning(sprintf("Skipping override row %d: file '%s' matched %d rows", i, file_key, length(idx_file)))
          n_skipped <- n_skipped + 1L
          next
        }
        idx <- idx_file[1]
      } else {
        if (length(idx_zid) != 1) {
          warning(sprintf("Skipping override row %d: zid '%s' matched %d rows", i, zid_key, length(idx_zid)))
          n_skipped <- n_skipped + 1L
          next
        }
        idx <- idx_zid[1]
      }

      if (kind == "exclude") {
        progress$excluded[idx] <- TRUE
        progress$reviewed[idx] <- TRUE
        message(sprintf("Applied override row %d: file=%s excluded", i, as.character(progress$file[idx])))
        n_applied <- n_applied + 1L
        next
      }
      if (kind == "ok") {
        progress$reviewed[idx] <- TRUE
        message(sprintf("Applied override row %d: file=%s zid=%s checked, no change",
                        i, as.character(progress$file[idx]), as.character(progress$zid[idx])))
        n_applied <- n_applied + 1L
        next
      }
      if (kind == "zid") {
        new_zid <- tolower(trimws(as.character(ov$response)))
        if (is.na(new_zid) || !grepl(zid_pattern, new_zid)) {
          warning(sprintf("Skipping override row %d: '%s' is not a valid %s",
                          i, ov$response, cfg$id$label))
          n_skipped <- n_skipped + 1L
          next
        }
        message(sprintf("Applied override row %d: file=%s zid %s -> %s",
                        i, as.character(progress$file[idx]), as.character(progress$zid[idx]), new_zid))
        progress$zid[idx] <- new_zid
        progress$reviewed[idx] <- TRUE
        n_applied <- n_applied + 1L
        next
      }

      resp_raw <- toupper(trimws(as.character(ov$response)))
      resp <- if (is.na(resp_raw) || resp_raw == "") NA_character_ else resp_raw

      if (!is.na(resp) && !resp %in% cfg$options) {
        warning(sprintf("Skipping override row %d: invalid response '%s'", i, ov$response))
        n_skipped <- n_skipped + 1L
        next
      }

      old_val <- progress[[q_col]][idx]
      progress[[q_col]][idx] <- resp
      progress$reviewed[idx] <- TRUE

      old_print <- if (is.na(old_val) || nchar(trimws(as.character(old_val))) == 0) "<blank>" else toupper(trimws(as.character(old_val)))
      new_print <- if (is.na(resp)) "<blank>" else resp
      message(sprintf("Applied override row %d: file=%s zid=%s Q%d %s -> %s",
                      i,
                      as.character(progress$file[idx]),
                      as.character(progress$zid[idx]),
                      q_raw,
                      old_print,
                      new_print))

      n_applied <- n_applied + 1L
    }

    message(sprintf("Overrides applied: %d  |  skipped: %d", n_applied, n_skipped))
  }



  # ---------------------------------------------------------------------------
  # What still needs a person, after overrides. A sheet the marker flagged is
  # cleared once any override row has been entered for it (the reviewer looked
  # at it). Independently of that, a sheet always needs review while it holds
  # an answer the marker was unsure of ("B*") or a zID that is not a valid ID:
  # those must be corrected by an override, not waved through.
  # ---------------------------------------------------------------------------
  q_cols_all <- paste0("q", EXPECTED_QUESTIONS)
  unsure_answer <- vapply(seq_len(nrow(progress)), function(i) {
    any(grepl("\\*$", vapply(q_cols_all, function(qc) as.character(progress[[qc]][i]), character(1))))
  }, logical(1))
  bad_zid <- is.na(progress$zid) | !grepl(zid_pattern, as.character(progress$zid))
  progress$needs_review <- ((progress$needs_review & !progress$reviewed) |
    unsure_answer | bad_zid) & !progress$excluded
  still <- character(nrow(progress))
  still[unsure_answer & progress$reviewed] <- "reviewed, but an uncertain answer (*) still needs an override"
  still[bad_zid & progress$reviewed] <- "reviewed, but the zID is still not valid"
  old_notes <- ifelse(is.na(progress$notes), "", as.character(progress$notes))
  progress$notes <- ifelse(nzchar(still),
                           ifelse(nzchar(old_notes), paste(still, old_notes, sep = " | "), still),
                           old_notes)

  message("Reading answer key:   ", key_path)
  key <- readr::read_csv(key_path, show_col_types = FALSE)
  key <- key[key$answer_master %in% cfg$options, , drop = FALSE]
  validate_answer_key(cfg, key)

  # ---------------------------------------------------------------------------
  # Score rows (one output row per row in progress.csv)
  # ---------------------------------------------------------------------------
  q_col_names <- paste0("q", EXPECTED_QUESTIONS)

  # ---------------------------------------------------------------------------
  # Score all students (score when possible, NA otherwise)
  # ---------------------------------------------------------------------------
  n_done <- sum(progress$status == "done", na.rm = TRUE)
  message(sprintf("Students with status='done': %d", n_done))
  bad_versions <- progress$status == "done" &
    (is.na(progress$exam_version) | !as.character(progress$exam_version) %in% VALID_VERSIONS)
  if (any(bad_versions, na.rm = TRUE)) {
    warning(sprintf(
      "%d done row(s) have missing/invalid exam_version and will not be scored. Review the marked images and progress.csv.",
      sum(bad_versions, na.rm = TRUE)), call. = FALSE)
  }

  # The answers as read (after overrides), e.g. "C A B C E* -": what a reviewer
  # compares against the paper. "-" is unanswered; "*" marks an uncertain read.
  answers_read <- function(student) {
    a <- vapply(q_col_names, function(qc) as.character(student[[qc]]), character(1))
    paste(ifelse(is.na(a) | !nzchar(a), "-", a), collapse = " ")
  }

  review_cols <- function(student) {
    data.frame(answers      = answers_read(student),
               files        = as.character(student$files %||% basename(student$file)),
               needs_review = isTRUE(student$needs_review),
               reviewed     = isTRUE(student$reviewed),
               excluded     = isTRUE(student$excluded),
               notes        = as.character(student$notes %||% NA_character_),
               stringsAsFactors = FALSE)
  }

  make_stub_row <- function(student) {
    row <- data.frame(
      file         = student$file,
      page         = student$page,
      zid          = student$zid,
      name         = student$name,
      exam_version = as.character(student$exam_version),
      score        = NA_real_,
      marks        = NA_real_,
      pct          = NA_real_,
      stringsAsFactors = FALSE
    )
    row <- cbind(row, review_cols(student))
    row$needs_review <- TRUE
    for (nm in q_col_names) row[[nm]] <- NA_integer_
    row
  }

  score_rows <- lapply(seq_len(nrow(progress)), function(i) {
    student <- progress[i, ]
    v       <- student$exam_version

    if (student$status != "done" || is.na(v) || !v %in% VALID_VERSIONS) {
      return(make_stub_row(student))
    }

    col_name <- paste0("answer_v", v)
    if (!col_name %in% names(key)) {
      warning(sprintf("No key column '%s' for zid=%s; skipping.", col_name, student$zid))
      return(make_stub_row(student))
    }

    score <- 0L
    marks <- 0
    q_vals <- stats::setNames(vector("integer", length(EXPECTED_QUESTIONS)), q_col_names)
    for (j in seq_along(EXPECTED_QUESTIONS)) {
      q       <- EXPECTED_QUESTIONS[j]
      col     <- q_col_names[j]
      raw_ans     <- student[[col]]
      student_ans <- if (is.na(raw_ans)) NA_character_ else toupper(trimws(as.character(raw_ans)))
      key_ans     <- toupper(trimws(key[[col_name]][key$question == q]))
      correct <- length(key_ans) == 1 && !is.na(student_ans) && student_ans == key_ans
      q_vals[col] <- if (is.na(student_ans)) 0 else as.integer(correct)
      if (isTRUE(correct)) {
        score <- score + 1L
        marks <- marks + marks_per_q[[as.character(q)]]
      }
    }

    row <- data.frame(
      file         = student$file,
      page         = student$page,
      zid          = student$zid,
      name         = student$name,
      exam_version = as.character(v),
      score        = score,
      marks        = marks,
      pct          = round(score / length(EXPECTED_QUESTIONS) * 100, 1),
      stringsAsFactors = FALSE
    )
    row <- cbind(row, review_cols(student))
    for (nm in q_col_names) row[[nm]] <- q_vals[[nm]]
    row
  })
  score_rows <- dplyr::bind_rows(score_rows)

  # ---------------------------------------------------------------------------
  # Combine and write output
  # ---------------------------------------------------------------------------
  updated_responses_path <- file.path(dir, "responses-updated.csv")
  key_row <- progress[NA_integer_, ]          # same columns, all NA
  key_row[1, "file"]    <- "ANSWER_KEY"
  key_row[1, "zid"]     <- ""
  key_row[1, "name"]    <- ""
  for (j in seq_along(EXPECTED_QUESTIONS)) {
    q   <- EXPECTED_QUESTIONS[j]
    col <- q_col_names[j]
    ans <- key$answer_master[key$question == q]
    key_row[1, col] <- if (length(ans) == 1) toupper(trimws(ans)) else NA_character_
  }
  readr::write_csv(dplyr::bind_rows(key_row, progress), updated_responses_path, na = "")
  message("Updated responses written: ", updated_responses_path)


  results <- score_rows

  readr::write_csv(results, output)
  message("Results written: ", output)

  # ---------------------------------------------------------------------------
  # Summary
  # ---------------------------------------------------------------------------
  scored <- score_rows[!is.na(score_rows$score), , drop = FALSE]
  n_scored <- nrow(scored)
  cat("\n========================================\n")
  cat(sprintf("Students scored : %d / %d\n", n_scored, nrow(score_rows)))
  cat(sprintf("Need review     : %d  (see the needs_review and notes columns)\n",
              sum(score_rows$needs_review)))

  if (n_scored > 0) {
    cat(sprintf("Mean score      : %.1f / %d  (%.1f%%)\n",
                mean(scored$score), length(EXPECTED_QUESTIONS), mean(scored$pct)))

    # Simple text histogram
    n_q    <- length(EXPECTED_QUESTIONS)
    step   <- max(1, round(n_q / 5))
    breaks <- unique(seq(0, n_q, by = step))
    cats   <- cut(scored$score,
                  breaks          = c(breaks, Inf),
                  right           = FALSE,
                  include.lowest  = TRUE)
    tbl <- table(cats)
    cat("\nScore distribution:\n")
    for (k in seq_along(tbl)) {
      bar <- paste(rep("#", tbl[[k]]), collapse = "")
      cat(sprintf("  %-12s | %s (%d)\n", names(tbl)[k], bar, tbl[[k]]))
    }
  }

  cat("========================================\n")

  invisible(results)
}
