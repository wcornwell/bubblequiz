# calibrate.R -- locate every bubble in the rendered answer sheet and write the
# coordinates the marker uses to read scans.
#
# The marker needs to know where each bubble sits on the page. Rather than
# trusting the LaTeX source, this reads the RENDERED PDF: it pulls the position
# of every printed option letter, clusters those into columns and rows, and
# checks the result against the exam config before writing it out. Re-run it
# whenever the sheet layout changes.

# A4 page dimensions in points
PAGE_W <- 595.28
PAGE_H <- 841.89

#' Calibrate bubble coordinates from the rendered answer sheet
#'
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param pdf Path to the blank rendered bubble sheet (any version).
#' @param out Path for the generated layout file.
#' @param preview Path for an annotated preview JPEG, or NULL to skip it.
#' @return Invisibly, the path to the generated layout file.
#' @export
calibrate_coords <- function(config  = default_config_path(),
                             pdf     = "output/bubblesheet_v1.pdf",
                             out     = "output/layout.R",
                             preview = "output/layout_preview.jpeg") {
  cfg <- if (is.list(config)) config else load_exam_config(config)

  if (!file.exists(pdf)) {
    stop("Blank bubble sheet PDF not found: ", pdf,
         "\nRun make_bubblesheet() first.", call. = FALSE)
  }
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

  n_opt  <- length(cfg$options)
  n_cols <- cfg$n_cols

  # --- 1. Text with positions, normalised to 0-1 ---------------------------
  # Every page is read, not just the first: an inline quiz form runs to as many
  # pages as the questions need, and the bubble rows on page 2 are as real as
  # those on page 1. Rows are collected in page order and each keeps the page it
  # was found on, so the marker can look for a question on the right sheet side.
  message("Reading PDF text data from: ", pdf)
  all_pages <- pdftools::pdf_data(pdf, font_info = TRUE)
  message("Pages in form: ", length(all_pages))

  page_rows <- list()
  x_centers <- NULL
  n_x <- NA_integer_

  for (pg in seq_along(all_pages)) {
    txt <- all_pages[[pg]]
    if (nrow(txt) == 0) next
    txt$xc <- (txt$x + txt$width  / 2) / PAGE_W
    txt$yc <- (txt$y + txt$height / 2) / PAGE_H

    # --- 2. Keep the option letters, at the small bubble font size ---------
    bubble_letters <- txt[txt$text %in% cfg$options, , drop = FALSE]
    if (nrow(bubble_letters) == 0) next

    # The ID-grid digits and section headings also use small fonts, so filter
    # relative to the median rather than against an absolute point size.
    med_size <- stats::median(bubble_letters$font_size, na.rm = TRUE)
    bubbles  <- bubble_letters[bubble_letters$font_size <= med_size * 1.2, , drop = FALSE]
    if (nrow(bubbles) == 0) next

    # --- 3. Cluster x positions into n_opt x n_cols groups ----------------
    x_sorted  <- sort(unique(round(bubbles$xc, 4)))
    x_group   <- cumsum(c(TRUE, diff(x_sorted) > 0.015))
    pg_x      <- tapply(x_sorted, x_group, mean)

    # --- 4. Cluster y positions into rows ---------------------------------
    y_sorted <- sort(unique(round(bubbles$yc, 4)))
    y_grp    <- cumsum(c(TRUE, diff(y_sorted) > 0.008))

    b_y   <- round(bubbles$yc, 4)
    b_gid <- y_grp[match(b_y, y_sorted)]
    agg   <- stats::aggregate(list(n = seq_along(b_gid)),
                              by = list(gid = b_gid), FUN = length)
    y_mean <- stats::aggregate(list(y_mean = bubbles$yc), by = list(gid = b_gid), FUN = mean)
    rows_pg <- merge(agg[, c("gid", "n")], y_mean, by = "gid")
    # A real answer row carries most of a full set of option letters.
    rows_pg <- rows_pg[rows_pg$n >= max(2L, n_opt - 1L), , drop = FALSE]
    rows_pg <- rows_pg[order(rows_pg$y_mean), , drop = FALSE]
    if (nrow(rows_pg) == 0) next

    rows_pg$page <- pg
    page_rows[[length(page_rows) + 1L]] <- rows_pg

    message(sprintf("  page %d: %.1f pt option letters | %d bubble row(s), %d x-group(s)",
                    pg, med_size, nrow(rows_pg), length(pg_x)))

    # The bubble columns are printed at the same x on every page. Take them
    # from the first page that has answer rows and require the rest to agree,
    # so a stray match on a later page cannot quietly shift the coordinates.
    if (is.null(x_centers)) {
      x_centers <- pg_x
      n_x <- length(pg_x)
    } else if (length(pg_x) != n_x || max(abs(as.numeric(pg_x) - as.numeric(x_centers))) > 0.01) {
      stop(sprintf("Bubble columns on page %d do not line up with page 1. Rebuild the form before calibrating.", pg),
           call. = FALSE)
    }
  }

  if (length(page_rows) == 0) {
    stop("No option letters (", paste(cfg$options, collapse = ""),
         ") found in the PDF text layer.", call. = FALSE)
  }

  rows_found <- do.call(rbind, page_rows)
  n_rows <- nrow(rows_found)

  message(sprintf("Detected %d x-group(s) and %d bubble row(s) across %d page(s).",
                  n_x, n_rows, length(page_rows)))
  validate_layout_match(cfg, n_rows, n_x)

  # --- 5. Build ANSWER_X / QUESTION_Y --------------------------------------
  answer_x <- stats::setNames(
    lapply(seq_len(n_cols), function(ci) {
      idx <- ((ci - 1L) * n_opt + 1L):(ci * n_opt)
      stats::setNames(round(as.numeric(x_centers[idx]), 4), cfg$options)
    }),
    cfg$col_names
  )

  question_y <- numeric(0)
  question_page <- integer(0)
  for (i in seq_len(n_rows)) {
    y_val <- round(rows_found$y_mean[i], 4)
    pg_val <- as.integer(rows_found$page[i])
    r <- cfg$bubble_rows[[i]]
    for (col in names(r)) {
      q <- r[[col]]
      if (q %in% cfg$questions) {
        question_y[as.character(q)] <- y_val
        question_page[as.character(q)] <- pg_val
      }
    }
  }

  # --- 6. Write the layout file --------------------------------------------
  fmt_x <- function(v) paste(sprintf("%s = %.4f", names(v), v), collapse = ", ")
  qy_lines <- vapply(seq_len(n_rows), function(i) {
    r  <- cfg$bubble_rows[[i]]
    qs <- as.character(unlist(r[intersect(cfg$col_names, names(r))]))
    qs <- qs[qs %in% as.character(cfg$questions)]
    paste(sprintf('"%s"=%.4f', qs, question_y[qs]), collapse = ",")
  }, character(1))

  lines <- c(
    sprintf("# Bubble coordinates for %s -- GENERATED by bubblequiz::calibrate_coords().",
            cfg$course),
    sprintf("# Source: %s", pdf),
    "# Re-run `bubblequiz calibrate` whenever the sheet layout changes.",
    "",
    "# x position (normalised 0-1) of each option letter, by column",
    "ANSWER_X <- list(",
    paste0("  ", paste(sprintf("%s = c(%s)", names(answer_x),
                               vapply(answer_x, fmt_x, character(1))),
                       collapse = ",\n  ")),
    ")",
    "",
    "# Column each question's bubbles sit in",
    "QUESTION_COL <- c(",
    paste0("  ", paste(sprintf('"%s"="%s"', names(cfg$question_col), cfg$question_col),
                       collapse = ",")),
    ")",
    "",
    "# y position (normalised 0-1) of each question's bubble row",
    "# Questions sharing a row share a y value. y is measured within the page the",
    "# row is printed on, so it must be read together with QUESTION_PAGE.",
    paste0("QUESTION_Y <- c(\n  ", paste(qy_lines, collapse = ",\n  "), "\n)"),
    "",
    "# Which page of the form each question's bubble row is printed on.",
    paste0("QUESTION_PAGE <- c(\n  ",
           paste(sprintf('"%s"=%d', names(question_page), as.integer(question_page)),
                 collapse = ","),
           "\n)")
  )
  out_dir  <- normalizePath(dirname(out), mustWork = TRUE)
  pdf_abs  <- normalizePath(pdf, mustWork = TRUE)
  rel_pdf  <- if (startsWith(pdf_abs, paste0(out_dir, "/"))) substring(pdf_abs, nchar(out_dir) + 2) else pdf_abs
  lines <- c(lines,
    "",
    "# The blank form these coordinates were measured from. The marker re-reads it",
    "# to measure each bubble's unfilled ink, which is the baseline a filled bubble",
    "# is compared against.",
    sprintf('FORM_PDF <- "%s"', rel_pdf))
  writeLines(lines, out)
  message("Wrote: ", out)

  if (!is.null(preview)) {
    write_layout_preview(pdf, answer_x, question_y, cfg, preview)
  }
  invisible(out)
}

# Annotate the blank sheet with the coordinates just derived, so a human can
# confirm at a glance that every letter lands inside its printed circle.
write_layout_preview <- function(pdf, answer_x, question_y, cfg, preview) {
  message("Creating preview: ", preview)
  img  <- magick::image_read_pdf(pdf, density = 150)[1]
  info <- magick::image_info(img)
  w <- info$width
  h <- info$height
  sz <- as.integer(w * 0.015)

  for (q in cfg$questions) {
    q_chr <- as.character(q)
    col   <- cfg$question_col[[q_chr]]
    y_pos <- question_y[q_chr]
    if (is.null(col) || is.na(col) || is.na(y_pos)) next

    for (letter in cfg$options) {
      x_pos <- answer_x[[col]][[letter]]
      if (is.null(x_pos) || is.na(x_pos)) next
      cx <- as.integer(x_pos * w)
      cy <- as.integer(y_pos * h)
      img <- magick::image_annotate(
        img, letter, gravity = "NorthWest",
        location = sprintf("+%d+%d",
                           max(0L, cx - as.integer(sz * 0.63)),
                           max(0L, cy - as.integer(sz * 1.25))),
        color = "red", size = sz, weight = 700)
    }
  }
  magick::image_write(img, preview, format = "jpeg", quality = 90)
  message("Wrote: ", preview)
  invisible(preview)
}
