# marker.R -- read scanned bubble sheets with the Claude vision API.
#
# The API does the reading; the pixels do the checking. Every page is also
# scanned directly for ink at each bubble centre, and an answer the model
# reports is overridden when the pixels clearly disagree (see
# enforce_blank_rows_from_scan). Each page is written out annotated so a human
# can audit what was recorded.
#
# Nothing here knows about a particular exam: question numbers, option letters
# and the vision prompt all come from the config, and bubble coordinates come
# from the calibrated layout file.

# Registration-mark centres in template space (normalised 0-1): 5mm squares
# inset 3mm from each corner of an A4 page, as drawn by the sheet preamble.
# Static page geometry, independent of the exam config.
REG_MARKERS_REF <- rbind(
  tl = c(x = 5.5 / 210,   y = 5.5 / 297),
  tr = c(x = 204.5 / 210, y = 5.5 / 297),
  bl = c(x = 5.5 / 210,   y = 291.5 / 297),
  br = c(x = 204.5 / 210, y = 291.5 / 297)
)

#' Load a calibrated layout file
#'
#' @param path Path to the layout file written by [calibrate_coords()].
#' @return A list with `ANSWER_X`, `QUESTION_COL` and `QUESTION_Y`.
#' @export
load_layout <- function(path = "output/layout.R") {
  if (!file.exists(path)) {
    stop("Calibrated layout not found: ", path,
         "\nRun `bubblequiz calibrate` first.", call. = FALSE)
  }
  e <- new.env(parent = emptyenv())
  sys.source(path, envir = e)
  missing <- setdiff(c("ANSWER_X", "QUESTION_COL", "QUESTION_Y"), ls(e))
  if (length(missing) > 0) {
    stop("Layout file is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  list(ANSWER_X = e$ANSWER_X, QUESTION_COL = e$QUESTION_COL, QUESTION_Y = e$QUESTION_Y)
}

# ---------------------------------------------------------------------------
# call_claude: call Claude vision API for one page image
# ---------------------------------------------------------------------------
call_claude <- function(image_path, model, api_key, prompt) {
  img_b64  <- base64enc::base64encode(image_path)
  ext <- tolower(tools::file_ext(image_path))
  img_type <- switch(ext,
    "png" = "image/png",
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    "webp" = "image/webp",
    "gif" = "image/gif",
    "image/jpeg"
  )

  body <- list(
    model      = model,
    max_tokens = 2048,
    messages   = list(
      list(
        role    = "user",
        content = list(
          list(
            type  = "image",
            source = list(
              type       = "base64",
              media_type = img_type,
              data       = img_b64
            )
          ),
          list(type = "text", text = prompt)
        )
      )
    )
  )

  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_retry(
      max_tries = 5,
      is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 529),
      backoff = \(i) min(2^i, 60)
    ) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp)
  raw_text <- parsed$content[[1]]$text
  raw_text
}

# ---------------------------------------------------------------------------
# parse_response: parse and validate Claude's JSON response
# ---------------------------------------------------------------------------
parse_response <- function(raw_text, page_num, cfg) {
  result <- list(
    page         = page_num,
    ok           = FALSE,
    zid          = NA_character_,
    name         = NA_character_,
    exam_version = NA_character_,
    answers      = NULL,
    confidence   = NA_character_,
    notes        = NA_character_,
    error        = NA_character_
  )

  # Strip markdown fences if Claude wrapped the JSON anyway
  clean <- trimws(raw_text)
  clean <- sub("^```(?:json)?\\s*", "", clean, perl = TRUE)
  clean <- sub("\\s*```$", "", clean, perl = TRUE)

  parsed <- tryCatch(jsonlite::fromJSON(clean, simplifyVector = TRUE), error = function(e) NULL)

  if (is.null(parsed)) {
    result$error <- paste("JSON parse failed:", substr(clean, 1, 200))
    return(result)
  }

  version <- as.character(parsed$exam_version)
  if (!version %in% cfg$valid_versions) {
    result$error <- paste("Invalid exam_version:", version)
    result$exam_version <- version
    return(result)
  }

  answers <- parsed$answers
  got_qs  <- as.integer(names(answers))
  missing  <- setdiff(cfg$questions, got_qs)
  extra    <- setdiff(got_qs, cfg$questions)

  if (length(missing) > 0 || length(extra) > 0) {
    result$error <- paste(
      "Answer count mismatch. Missing:", paste(missing, collapse = ","),
      "Extra:", paste(extra, collapse = ",")
    )
    return(result)
  }

  # Normalise answers: allow A-E, A-E with trailing *, or "" (blank row)
  blank_qs <- integer(0)
  for (q in cfg$questions) {
    q_chr <- as.character(q)
    ans <- answers[[q_chr]]
    if (is.null(ans) || length(ans) == 0 || is.na(ans)) {
      ans <- ""
    }
    ans <- toupper(trimws(as.character(ans)[1]))

    if (ans == "") {
      blank_qs <- c(blank_qs, q)
      answers[[q_chr]] <- ""
      next
    }

    base_ans <- sub("\\*$", "", ans)
    star_matches <- gregexpr("\\*", ans, perl = TRUE)[[1]]
    star_count <- if (identical(star_matches[1], -1L)) 0L else length(star_matches)
    has_trailing_star <- grepl("\\*$", ans)

    if (!base_ans %in% cfg$options ||
        (star_count > 0L && !has_trailing_star) ||
        star_count > 1L) {
      result$error <- paste("Invalid answer for Q", q_chr, ":", ans)
      return(result)
    }

    answers[[q_chr]] <- if (has_trailing_star) paste0(base_ans, "*") else base_ans
  }

  result$ok            <- TRUE
  result$zid           <- as.character(parsed$zid)
  result$name          <- if (!is.null(parsed$name)) trimws(as.character(parsed$name)) else NA_character_
  result$exam_version  <- version
  result$answers       <- answers
  result$confidence    <- as.character(parsed$confidence)
  result$notes         <- if (!is.null(parsed$notes)) as.character(parsed$notes) else ""

  # Record blank rows in notes while keeping per-question uncertainty in answers.
  if (length(blank_qs) > 0) {
    blank_note <- paste0("Blank answer rows: Q", paste(blank_qs, collapse = ", Q"))
    if (nchar(trimws(result$notes)) == 0) {
      result$notes <- blank_note
    } else {
      result$notes <- paste(result$notes, blank_note, sep = " | ")
    }
  }
  result
}

# ---------------------------------------------------------------------------
# save_progress: write progress CSV to disk
# ---------------------------------------------------------------------------
save_progress <- function(progress, idx, csv_path) {
  readr::write_csv(progress, csv_path)
  invisible(progress)
}

# ---------------------------------------------------------------------------
# gray_matrix: convert image to grayscale matrix (0-255; lower is darker)
# ---------------------------------------------------------------------------
gray_matrix <- function(img) {
  g <- magick::image_convert(img, colorspace = "gray")
  arr <- magick::image_data(g, channels = "gray")
  vals <- suppressWarnings(strtoi(arr[1, , ], base = 16L))
  mat  <- matrix(vals, nrow = dim(arr)[2], ncol = dim(arr)[3], byrow = FALSE)
  t(mat)
}

# ---------------------------------------------------------------------------
# detect_corner_markers: estimate marker centers in scan-space using darkest
# pixels near expected corner locations.
# Returns matrix with rows tl/tr/bl/br and cols x/y.
# ---------------------------------------------------------------------------
detect_corner_markers <- function(gray, w, h) {
  thresh <- min(80L, as.integer(stats::quantile(gray, probs = 0.12, na.rm = TRUE)))

  find_one <- function(ref_xy) {
    cx <- as.integer(ref_xy["x"] * w)
    cy <- as.integer(ref_xy["y"] * h)
    rx <- max(20L, as.integer(w * 0.08))
    ry <- max(20L, as.integer(h * 0.08))

    x1 <- max(1L, cx - rx)
    x2 <- min(w,  cx + rx)
    y1 <- max(1L, cy - ry)
    y2 <- min(h,  cy + ry)

    win <- gray[y1:y2, x1:x2, drop = FALSE]
    dark_idx <- which(win <= thresh, arr.ind = TRUE)

    if (nrow(dark_idx) < 20) return(c(x = NA_real_, y = NA_real_))

    x_abs <- x1 + dark_idx[, "col"] - 1L
    y_abs <- y1 + dark_idx[, "row"] - 1L

    # Keep pixels closest to expected center to avoid nearby printed text.
    d2 <- (x_abs - cx)^2 + (y_abs - cy)^2
    keep_n <- max(30L, min(length(d2), as.integer(length(d2) * 0.2)))
    keep <- order(d2)[seq_len(keep_n)]

    c(x = stats::median(x_abs[keep]), y = stats::median(y_abs[keep]))
  }

  pts <- t(vapply(seq_len(nrow(REG_MARKERS_REF)), function(i) {
    find_one(REG_MARKERS_REF[i, ])
  }, numeric(2)))

  rownames(pts) <- rownames(REG_MARKERS_REF)
  colnames(pts) <- c("x", "y")

  # Basic geometry sanity checks before using these points.
  if (any(!is.finite(pts))) return(NULL)
  if (pts["tr", "x"] <= pts["tl", "x"] || pts["br", "x"] <= pts["bl", "x"]) return(NULL)
  if (pts["bl", "y"] <= pts["tl", "y"] || pts["br", "y"] <= pts["tr", "y"]) return(NULL)
  pts
}

# ---------------------------------------------------------------------------
# fit_bilinear_map: fit bilinear map from template-space (u,v) to image pixels (x,y)
# x = ax0 + ax1*u + ax2*v + ax3*u*v ; same for y
# ---------------------------------------------------------------------------
fit_bilinear_map <- function(src_uv, dst_xy) {
  M <- cbind(1, src_uv[, "x"], src_uv[, "y"], src_uv[, "x"] * src_uv[, "y"])
  ax <- as.numeric(solve(M, dst_xy[, "x"]))
  ay <- as.numeric(solve(M, dst_xy[, "y"]))

  function(u, v) {
    b <- c(1, u, v, u * v)
    c(x = sum(ax * b), y = sum(ay * b))
  }
}

# ---------------------------------------------------------------------------
# build_map_xy: build a coordinate mapping function from an image
# ---------------------------------------------------------------------------
build_map_xy <- function(img_path) {
  img  <- magick::image_read(img_path)
  info <- magick::image_info(img)
  w    <- info$width
  h    <- info$height
  gray <- gray_matrix(img)
  marker_pts <- detect_corner_markers(gray, w, h)

  list(
    img        = img,
    w          = w,
    h          = h,
    gray       = gray,
    map_xy     = if (!is.null(marker_pts)) {
      fit_bilinear_map(REG_MARKERS_REF, marker_pts)
    } else {
      function(u, v) c(x = u * w, y = v * h)
    },
    marker_ok  = !is.null(marker_pts)
  )
}

# ---------------------------------------------------------------------------
# enforce_blank_rows_from_scan: detect likely blank answer rows directly from
# scan pixels. If a row has no dark center in any bubble, force answer to "".
# ---------------------------------------------------------------------------
enforce_blank_rows_from_scan <- function(img_path, parsed, cfg, layout) {
  if (!isTRUE(parsed$ok)) return(parsed)

  ctx    <- build_map_xy(img_path)
  w      <- ctx$w
  h      <- ctx$h
  gray   <- ctx$gray
  map_xy <- ctx$map_xy

  center_r <- max(2L, as.integer(w * 0.0022))
  center_ink <- function(cx, cy) {
    x1 <- max(1L, cx - center_r)
    x2 <- min(w,  cx + center_r)
    y1 <- max(1L, cy - center_r)
    y2 <- min(h,  cy + center_r)
    255 - mean(gray[y1:y2, x1:x2, drop = FALSE], na.rm = TRUE)
  }

  forced_blank <- integer(0)

  for (q in cfg$questions) {
    q_chr <- as.character(q)
    col   <- layout$QUESTION_COL[q_chr]
    y_pos <- layout$QUESTION_Y[q_chr]
    if (is.null(col) || is.null(y_pos) || is.na(col) || is.na(y_pos)) next

    inks <- vapply(cfg$options, function(letter) {
      x_pos <- layout$ANSWER_X[[col]][letter]
      if (is.null(x_pos) || is.na(x_pos)) return(0)
      pt <- map_xy(x_pos, y_pos)
      cx <- as.integer(pt["x"])
      cy <- as.integer(pt["y"])
      center_ink(cx, cy)
    }, numeric(1))

    if (!all(is.finite(inks))) next

    ranked <- sort(inks, decreasing = TRUE)
    top_ink <- ranked[1]
    second_ink <- ranked[2]
    spread <- max(inks, na.rm = TRUE) - min(inks, na.rm = TRUE)
    top_letter <- names(which.max(inks))[1]

    raw_ans <- parsed$answers[[q_chr]]
    ans_base <- toupper(trimws(sub("\\*$", "", as.character(raw_ans)[1])))
    model_has_letter <- nzchar(ans_base) && ans_base %in% cfg$options
    model_disagrees <- model_has_letter && ans_base != top_letter

    # Rule 1: all centers are very light -> likely genuinely blank row.
    low_signal_blank <- top_ink < 65

    # Rule 2: no clear dominant bubble and model choice disagrees with pixel winner.
    # Keep this conservative so darker/strongly marked rows are not blanked.
    weak_dominance <- (top_ink - second_ink) < 20 && spread < 36 && top_ink < 141

    if (low_signal_blank || (weak_dominance && model_disagrees)) {
      parsed$answers[[q_chr]] <- ""
      forced_blank <- c(forced_blank, q)
    }
  }

  if (length(forced_blank) > 0) {
    add_note <- paste0("Pixel blank-check forced blank: Q", paste(forced_blank, collapse = ", Q"))
    if (is.null(parsed$notes) || nchar(trimws(parsed$notes)) == 0) {
      parsed$notes <- add_note
    } else if (!grepl(add_note, parsed$notes, fixed = TRUE)) {
      parsed$notes <- paste(parsed$notes, add_note, sep = " | ")
    }
  }

  parsed
}

# ---------------------------------------------------------------------------
# annotate_page: write an annotated copy of a page to <dir>/marked/
# Draws the detected answer letter (A-E) at each bubble's position using
# BUBBLE_LAYOUT coordinates.
# ---------------------------------------------------------------------------
annotate_page <- function(img_path, parsed, marked_dir, cfg = NULL, layout = NULL) {
  ctx    <- build_map_xy(img_path)
  img    <- ctx$img
  w      <- ctx$w
  map_xy <- ctx$map_xy
  sz     <- as.integer(w * 0.021)
  bubble_half <- as.integer(w * (8 / 595.28))
  text_annotation_enabled <- TRUE

  if (!ctx$marker_ok) {
    message("    [annotate] marker detection failed, using simple width/height scaling")
  }

  if (isTRUE(parsed$ok) && !is.null(cfg) && !is.null(layout)) {
    for (q in cfg$questions) {
      q_chr  <- as.character(q)
      letter_raw <- toupper(trimws(parsed$answers[[q_chr]]))
      if (is.na(letter_raw) || nchar(letter_raw) == 0) next

      is_uncertain <- grepl("\\*$", letter_raw)
      letter <- sub("\\*$", "", letter_raw)
      if (!letter %in% cfg$options) next

      col   <- layout$QUESTION_COL[q_chr]
      x_pos <- layout$ANSWER_X[[col]][letter]
      y_pos <- layout$QUESTION_Y[q_chr]

      if (is.null(col) || is.null(x_pos) || is.null(y_pos) ||
          is.na(col)   || is.na(x_pos)   || is.na(y_pos)) {
        message(sprintf("    [annotate] no coords for Q%s letter %s -- skipped", q_chr, letter))
        next
      }

      pt    <- map_xy(x_pos, y_pos)
      cx    <- as.integer(pt["x"])
      cy    <- as.integer(pt["y"])
      x_off <- max(0L, cx - as.integer(sz * 0.63) + bubble_half)
      y_off <- max(0L, cy - as.integer(sz * 1.25) + bubble_half)
      annotate_location <- sprintf("+%d+%d", x_off, y_off)
      annotate_color <- if (is_uncertain) "red" else "orange"

      if (isTRUE(text_annotation_enabled)) {
        img <- tryCatch(
          magick::image_annotate(img, letter,
                         gravity   = "NorthWest",
                         location  = annotate_location,
                         color     = annotate_color,
                         size      = sz,
                         weight    = 700),
          error = function(e) {
            message("    [annotate] text render failed; switching to square markers")
            text_annotation_enabled <<- FALSE
            marker <- magick::image_blank(width = max(6L, as.integer(sz * 0.7)),
                                  height = max(6L, as.integer(sz * 0.7)),
                                  color = annotate_color)
            magick::image_composite(img, marker, offset = annotate_location, operator = "over")
          }
        )
      } else {
        marker <- magick::image_blank(width = max(6L, as.integer(sz * 0.7)),
                              height = max(6L, as.integer(sz * 0.7)),
                              color = annotate_color)
        img <- magick::image_composite(img, marker, offset = annotate_location, operator = "over")
      }
    }
  }

  out_name <- sub("\\.jpe?g$", "-marked.jpeg", basename(img_path), ignore.case = TRUE)
  magick::image_write(img, file.path(marked_dir, out_name), format = "jpeg", quality = 85)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# update_progress_row: write parsed results back into the progress data frame
# ---------------------------------------------------------------------------
update_progress_row <- function(progress, idx, parsed, cfg, api_call_ok = TRUE) {
  if (!parsed$ok) {
    progress$status[idx]       <- "error"
    progress$api_call_ok[idx]  <- api_call_ok
    progress$needs_review[idx] <- TRUE
    progress$error[idx]        <- parsed$error
    progress$zid[idx]          <- parsed$zid
    progress$name[idx]         <- parsed$name
    progress$exam_version[idx] <- parsed$exam_version
    return(progress)
  }

  progress$status[idx]       <- "done"
  progress$api_call_ok[idx]  <- TRUE
  has_uncertain <- any(grepl("\\*$", as.character(parsed$answers), perl = TRUE), na.rm = TRUE)
  progress$needs_review[idx] <- isTRUE(has_uncertain)
  progress$zid[idx]          <- parsed$zid
  progress$name[idx]         <- parsed$name
  progress$exam_version[idx] <- parsed$exam_version
  progress$confidence[idx]   <- parsed$confidence
  progress$notes[idx]        <- parsed$notes
  progress$error[idx]        <- NA_character_

  for (q in cfg$questions) {
    ans <- parsed$answers[[as.character(q)]]
    progress[[paste0("q", q)]][idx] <- if (is.null(ans) || is.na(ans) || nchar(trimws(ans)) == 0) {
      NA_character_
    } else {
      toupper(trimws(ans))
    }
  }

  progress
}

# ---------------------------------------------------------------------------
# mark_scans: main processing loop
# ---------------------------------------------------------------------------

#' Mark scanned bubble sheets
#'
#' Works through `progress.csv` a page at a time, saving after every page, so
#' an interrupted run resumes where it stopped: pages already marked `done` are
#' skipped on the next call. Every page is also written to `marked/` with the
#' recorded answer drawn on each bubble (orange = confident, red = uncertain).
#'
#' @param dir Folder created by [preprocess_scans()].
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param layout Path to the calibrated layout file, or a loaded layout list.
#' @param model Anthropic model ID.
#' @param api_key Anthropic API key; defaults to `$ANTHROPIC_API_KEY`.
#' @param dry_run Process only the first 3 pending pages.
#' @return Invisibly, the updated progress data frame.
#' @export
mark_scans <- function(dir,
                       config  = default_config_path(),
                       layout  = "output/layout.R",
                       model   = "claude-sonnet-4-6",
                       api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                       dry_run = FALSE) {
  if (!dir.exists(dir)) stop("Directory not found: ", dir, call. = FALSE)
  if (!nzchar(api_key)) {
    stop("ANTHROPIC_API_KEY is not set. Export it, or pass api_key =.", call. = FALSE)
  }
  cfg    <- if (is.list(config)) config else load_exam_config(config)
  layout <- if (is.list(layout)) layout else load_layout(layout)

  csv_path   <- file.path(dir, "progress.csv")
  if (!file.exists(csv_path)) {
    stop("progress.csv not found in ", dir,
         " -- run `bubblequiz preprocess` first.", call. = FALSE)
  }
  marked_dir <- file.path(dir, "marked")
  dir.create(marked_dir, showWarnings = FALSE)

  message("Loading progress CSV: ", csv_path)
  progress <- readr::read_csv(csv_path, show_col_types = FALSE)

  if (!("name" %in% names(progress))) {
    progress$name <- NA_character_
  }

  n_done     <- sum(progress$status == "done", na.rm = TRUE)
  to_process <- which(progress$status != "done")

  if (isTRUE(dry_run)) {
    message("-- dry run: processing the first 3 pending pages only --")
    to_process <- utils::head(to_process, 3)
  }

  message(sprintf("Pages total: %d  |  already done: %d  |  to process: %d",
                  nrow(progress), n_done, length(to_process)))

  for (idx in to_process) {
    page_num <- progress$page[idx]
    img_path <- file.path(dir, progress$file[idx])
    message(sprintf("  Page %d / %d  [%s] ...", page_num, nrow(progress), progress$file[idx]))

    raw <- tryCatch(
      call_claude(img_path, model, api_key, cfg$vision_prompt),
      error = function(e) {
        msg <- conditionMessage(e)
        if (inherits(e, "httr2_http")) {
          detail <- tryCatch(
            httr2::resp_body_json(e$resp)$error$message,
            error = function(e2) tryCatch(httr2::resp_body_string(e$resp), error = function(e3) NULL)
          )
          if (!is.null(detail)) msg <- paste0(msg, "\n      Detail: ", detail)
        }
        message("    API error: ", msg)
        NULL
      }
    )

    if (is.null(raw)) {
      progress$status[idx]       <- "error"
      progress$api_call_ok[idx]  <- FALSE
      progress$needs_review[idx] <- TRUE
      progress$error[idx]        <- "API call failed"
      progress <- save_progress(progress, idx, csv_path)
      tryCatch(
        annotate_page(img_path, list(ok = FALSE, error = "API call failed"), marked_dir,
                      cfg, layout),
        error = function(e) message("    [annotate] failed: ", conditionMessage(e))
      )
      next
    }

    parsed <- parse_response(raw, page_num, cfg)
    parsed <- enforce_blank_rows_from_scan(img_path, parsed, cfg, layout)

    if (!parsed$ok) {
      message("    Parse/validation error: ", parsed$error)
    } else {
      if (any(grepl("\\*$", as.character(parsed$answers), perl = TRUE), na.rm = TRUE)) {
        message("    Uncertain question(s) flagged with '*': ", parsed$notes)
      }
      message(sprintf("    zID: %s  version: %s", parsed$zid, parsed$exam_version))
    }

    progress <- update_progress_row(progress, idx, parsed, cfg)
    progress <- save_progress(progress, idx, csv_path)
    tryCatch(
      annotate_page(img_path, parsed, marked_dir, cfg, layout),
      error = function(e) message("    [annotate] failed: ", conditionMessage(e))
    )
  }

  # Summary
  results_df <- progress[progress$status == "done", , drop = FALSE]
  n_ok      <- nrow(results_df)
  n_error   <- sum(progress$status == "error",   na.rm = TRUE)
  n_review  <- sum(progress$needs_review == TRUE, na.rm = TRUE)

  cat("\n========================================\n")
  cat(sprintf("Students processed : %d\n", n_ok))
  cat(sprintf("Errors             : %d\n", n_error))
  cat(sprintf("Needs manual review: %d  (see needs_review column in progress.csv)\n", n_review))
  cat(sprintf("Next: bubblequiz score --dir \"%s\"\n", dir))
  cat("========================================\n")

  invisible(progress)
}
