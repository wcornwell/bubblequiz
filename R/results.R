# results.R -- score marked sheets against the answer key.
#
# Reads progress.csv (written by the marker), applies any manual corrections
# from overrides.csv, scores each student against their own exam version, and
# writes results.csv. Pages that never marked cleanly still get a row, with NA
# scores, so nobody silently disappears between the scanner and the gradebook.

#' Score marked bubble sheets against the answer key
#'
#' @param dir Folder created by [preprocess_scans()] and filled in by [mark_scans()].
#' @param key Path to `answer_key.csv` from [generate_versions()].
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param output Output CSV path; defaults to `results.csv` inside `dir`.
#' @return Invisibly, the results data frame.
#' @export
score_results <- function(dir,
                          key    = "output/answer_key.csv",
                          config = default_config_path(),
                          output = NULL) {
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
  message("Reading progress CSV: ", csv_path)
  progress <- readr::read_csv(csv_path, show_col_types = FALSE)
  if (!("name" %in% names(progress))) {
    progress$name <- NA_character_
  }


  if (file.exists(overrides_path)) {
    message("Reading overrides:    ", overrides_path)
    overrides <- readr::read_csv(overrides_path, show_col_types = FALSE)
    required_cols <- c("page", "zid", "name", "question", "response")
    has_override_file_col <- "file" %in% names(overrides)

    if (!all(required_cols %in% names(overrides))) {
      stop("Overrides file must contain columns: page, zid, name, question, response", call. = FALSE)
    }

    n_applied <- 0L
    n_skipped <- 0L

    for (i in seq_len(nrow(overrides))) {
      ov <- overrides[i, ]

      q_raw <- suppressWarnings(as.integer(trimws(as.character(ov$question))))
      if (is.na(q_raw) || !q_raw %in% EXPECTED_QUESTIONS) {
        warning(sprintf("Skipping override row %d: invalid question '%s'", i, ov$question))
        n_skipped <- n_skipped + 1L
        next
      }

      q_col <- paste0("q", q_raw)
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

      idx_file <- if (has_file) which(as.character(progress$file) == file_key) else integer(0)
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

      resp_raw <- toupper(trimws(as.character(ov$response)))
      resp <- if (is.na(resp_raw) || resp_raw == "") NA_character_ else resp_raw

      if (!is.na(resp) && !resp %in% cfg$options) {
        warning(sprintf("Skipping override row %d: invalid response '%s'", i, ov$response))
        n_skipped <- n_skipped + 1L
        next
      }

      old_val <- progress[[q_col]][idx]
      progress[[q_col]][idx] <- resp

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
