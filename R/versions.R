# versions.R -- shuffle option order to produce N versions of a question paper,
# and emit the answer key that the marker scores against.
#
# Source format (questions.md in the course repo):
#
#   **Question 7 [2 marks]:** What does ...?
#
#   a. first option
#   b. second option
#   c. third option
#
#   <!-- Answer C -- explanation, kept out of the rendered PDF -->
#
# Options may also appear on a single line separated by two or more spaces.
# Everything else in the file passes through untouched.

#' Option-order permutations for each exam version
#'
#' Each version applies ONE permutation to every question's options, so marking
#' needs only a single letter-mapping table per version rather than per question.
#'
#' `perm[i]` is the index of the original option that appears at new position i.
#'
#' For five options the first four permutations are the ones used by the
#' original BEES2041 exam, kept so existing answer keys reproduce exactly.
#' Beyond that (or for a different number of options) permutations are built
#' deterministically by rotating, and reversing alternate rotations, so the same
#' config always yields the same paper.
#'
#' @param n_options Number of answer options per question.
#' @param n_versions Number of versions to generate.
#' @return A named list of integer permutation vectors, `v1` .. `v<n_versions>`.
#' @export
option_permutations <- function(n_options, n_versions) {
  known5 <- list(
    c(1L, 2L, 3L, 4L, 5L),   # a b c d e  (unchanged -- reference version)
    c(3L, 5L, 1L, 4L, 2L),   # c e a d b
    c(5L, 3L, 2L, 1L, 4L),   # e c b a d
    c(2L, 4L, 5L, 3L, 1L)    # b d e c a
  )

  perms <- vector("list", n_versions)
  for (k in seq_len(n_versions)) {
    if (n_options == 5L && k <= length(known5)) {
      perms[[k]] <- known5[[k]]
    } else if (k == 1L) {
      perms[[k]] <- seq_len(n_options)
    } else {
      # Rotate by (k-1), reversing on alternate versions for more mixing.
      shift <- ((k - 1L) %% n_options)
      p <- c(seq.int(shift + 1L, n_options), seq_len(shift))[seq_len(n_options)]
      if (k %% 2L == 0L) p <- rev(p)
      perms[[k]] <- as.integer(p)
    }
  }
  names(perms) <- paste0("v", seq_len(n_versions))

  dup <- duplicated(vapply(perms, paste, character(1), collapse = "-"))
  if (any(dup)) {
    stop(sprintf(
      "Cannot build %d distinct versions from %d options -- reduce `versions:` in the exam config.",
      n_versions, n_options), call. = FALSE)
  }
  perms
}

# Rewrite the <!-- Answer X --> comment using the inverse permutation, so the
# stored answer is always the letter a student should select in that version.
update_answer_comment <- function(comment_line, inv, opts_lower) {
  cls <- paste0("[", paste(c(opts_lower, toupper(opts_lower)), collapse = ""), "]")
  orig_letter <- tolower(sub(paste0("<!-- Answer (", cls, ").*"), "\\1", comment_line))
  orig_pos    <- match(orig_letter, opts_lower)
  if (is.na(orig_pos)) return(comment_line)
  new_letter  <- toupper(opts_lower[inv[orig_pos]])
  sub("Answer .", paste0("Answer ", new_letter), comment_line)
}

# Walk every line, detect option blocks in either supported format, reorder the
# options and update the matching answer comment.
process_version <- function(lines, perm, version_label, opts_lower) {
  inv <- order(perm)
  n   <- length(opts_lower)
  cls <- paste0("[", paste(c(opts_lower, toupper(opts_lower)), collapse = ""), "]")
  out <- c(paste0("# Version ", version_label), "", lines)

  starts_with <- function(line, letter) grepl(paste0("^", letter, "\\.\\s"), line)

  i <- 1L
  while (i <= length(out)) {
    line <- out[i]

    # --- Format 1: one option per line, n consecutive lines ---
    multi <- starts_with(line, opts_lower[1]) &&
      i + n - 1L <= length(out) &&
      all(vapply(seq_len(n), function(k) starts_with(out[i + k - 1L], opts_lower[k]), logical(1)))

    single <- !multi && starts_with(line, opts_lower[1]) &&
      grepl(paste0("\\s{2,}", opts_lower[2], "\\.\\s"), line)

    if (multi) {
      block <- out[i:(i + n - 1L)]
      opt_texts <- sub(paste0("^", cls, "\\.\\s+"), "", block)
      out[i:(i + n - 1L)] <- paste0(opts_lower, ". ", opt_texts[perm])

      for (j in seq(i + n, min(i + n + 3L, length(out)))) {
        if (grepl(paste0("^<!-- Answer ", cls), out[j])) {
          out[j] <- update_answer_comment(out[j], inv, opts_lower)
          break
        }
      }
      i <- i + n

    } else if (single) {
      pattern <- paste0("\\s{2,}(?=[", paste(opts_lower[-1], collapse = ""), "]\\.\\s)")
      parts <- strsplit(line, pattern, perl = TRUE)[[1]]
      if (length(parts) == n) {
        opt_texts <- sub(paste0("^", cls, "\\.\\s+"), "", parts)
        out[i] <- paste(paste0(opts_lower, ". ", opt_texts[perm]), collapse = "   ")

        for (j in seq(i + 1L, min(i + 3L, length(out)))) {
          if (grepl(paste0("^<!-- Answer ", cls), out[j])) {
            out[j] <- update_answer_comment(out[j], inv, opts_lower)
            break
          }
        }
      }
      i <- i + 1L
    } else {
      i <- i + 1L
    }
  }
  out
}

# Extract question numbers and master answers from the source markdown.
extract_master_answers <- function(lines, opts_lower) {
  cls <- paste0("[", paste(c(opts_lower, toupper(opts_lower)), collapse = ""), "]")
  q_num  <- NA_integer_
  q_nums <- integer(0)
  q_ans  <- character(0)

  for (line in lines) {
    m <- regmatches(line, regexpr("\\*\\*Question (\\d+)", line))
    if (length(m) == 1L) {
      q_num <- as.integer(sub("\\*\\*Question (\\d+)", "\\1", m))
    }
    if (grepl("^<!-- Answer", line) && !is.na(q_num)) {
      raw <- sub("^<!-- Answer ([^ ]+).*", "\\1", line)
      q_nums <- c(q_nums, q_num)
      q_ans  <- c(q_ans, toupper(raw))
      q_num  <- NA_integer_   # reset so we don't double-count
    }
  }
  data.frame(question = q_nums, answer_master = q_ans, stringsAsFactors = FALSE)
}

#' Generate shuffled question-paper versions and the answer key
#'
#' Reads the course's `questions.md`, writes one shuffled markdown file per
#' version plus `answer_key.csv` (one row per question, one column per version)
#' and a human-readable `answer_key.md` of the letter mappings.
#'
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param questions Path to the question source markdown.
#' @param outdir Output directory.
#' @return Invisibly, the answer key data frame.
#' @export
generate_versions <- function(config = default_config_path(),
                              questions = "questions.md",
                              outdir = "output") {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  if (!file.exists(questions)) {
    stop("Question source not found: ", questions, call. = FALSE)
  }
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  opts_lower <- tolower(cfg$options)
  perms <- option_permutations(length(cfg$options), cfg$n_versions)
  lines <- readLines(questions, warn = FALSE)

  key_lines <- c("# Answer Key -- All Versions", "")

  for (vname in names(perms)) {
    perm  <- perms[[vname]]
    inv   <- order(perm)
    label <- sub("^v", "", vname)

    out  <- process_version(lines, perm, label, opts_lower)
    path <- file.path(outdir, paste0("questions_", vname, ".md"))
    writeLines(out, path)
    message("Wrote: ", path)

    key_lines <- c(key_lines,
      paste0("## Version ", label),
      "Original -> Version letter mapping:",
      paste(paste0(toupper(opts_lower), "->", toupper(opts_lower[inv])), collapse = "  "),
      ""
    )
  }

  key_path <- file.path(outdir, "answer_key.md")
  writeLines(key_lines, key_path)
  message("Wrote: ", key_path)

  df <- extract_master_answers(lines, opts_lower)

  remap_answer <- function(ans_master, perm) {
    inv <- order(perm)
    vapply(ans_master, function(a) {
      pos <- match(tolower(a), opts_lower)
      if (!is.na(pos)) toupper(opts_lower[inv[pos]]) else a
    }, character(1L), USE.NAMES = FALSE)
  }

  for (vname in names(perms)) {
    df[[paste0("answer_", vname)]] <- remap_answer(df$answer_master, perms[[vname]])
  }

  # Catch question-source / config drift before it reaches a live exam.
  mcq <- df[df$answer_master %in% toupper(opts_lower), , drop = FALSE]
  missing <- setdiff(cfg$questions, mcq$question)
  extra   <- setdiff(mcq$question, c(cfg$questions, cfg$essay_questions))
  if (length(missing) > 0 || length(extra) > 0) {
    warning(sprintf(
      "Question source does not match the exam config.\n  In config but not in %s: %s\n  In %s but not in config: %s",
      basename(questions), if (length(missing)) paste(missing, collapse = ", ") else "(none)",
      basename(questions), if (length(extra)) paste(extra, collapse = ", ") else "(none)"),
      call. = FALSE)
  }

  csv_path <- file.path(outdir, "answer_key.csv")
  utils::write.csv(df, csv_path, row.names = FALSE)
  message("Wrote: ", csv_path)
  invisible(df)
}
