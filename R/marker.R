# marker.R -- read scanned bubble sheets with local, deterministic computer
# vision. No network call, no API key: every bubble is read by sampling ink
# darkness at its calibrated center and classifying the darkest circle in each
# row. Each page is written out annotated so a human can audit what was
# recorded.
#
# Nothing here knows about a particular exam: question numbers and option
# letters come from the config, and bubble coordinates come from the
# calibrated layout file.

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
  e <- new.env(parent = baseenv())
  sys.source(path, envir = e)
  missing <- setdiff(c("ANSWER_X", "QUESTION_COL", "QUESTION_Y"), ls(e))
  if (length(missing) > 0) {
    stop("Layout file is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  # QUESTION_PAGE arrived with multi-page forms. A layout calibrated before
  # that is a single-page form, so treat every question as page 1.
  question_page <- if ("QUESTION_PAGE" %in% ls(e)) {
    e$QUESTION_PAGE
  } else {
    stats::setNames(rep(1L, length(e$QUESTION_Y)), names(e$QUESTION_Y))
  }
  form_pdf <- NULL
  if ("FORM_PDF" %in% ls(e)) {
    form_pdf <- if (startsWith(e$FORM_PDF, "/")) e$FORM_PDF
                else file.path(dirname(normalizePath(path, mustWork = TRUE)), e$FORM_PDF)
  }
  out <- list(ANSWER_X = e$ANSWER_X, QUESTION_COL = e$QUESTION_COL,
              QUESTION_Y = e$QUESTION_Y, QUESTION_PAGE = question_page,
              form_pdf = form_pdf)
  attr(out, "path") <- normalizePath(path, mustWork = TRUE)
  out
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
    u <- as.numeric(u)
    v <- as.numeric(v)
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
      function(u, v) c(x = as.numeric(u) * w, y = as.numeric(v) * h)
    },
    marker_ok  = !is.null(marker_pts)
  )
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

  out_name <- sub("\\.(jpe?g|png)$", "-marked.jpeg", basename(img_path), ignore.case = TRUE)
  magick::image_write(img, file.path(marked_dir, out_name), format = "jpeg", quality = 85)
  invisible(NULL)
}

ink_score <- function(gray, cx, cy, radius) {
  h <- nrow(gray)
  w <- ncol(gray)
  x1 <- max(1L, cx - radius)
  x2 <- min(w,  cx + radius)
  y1 <- max(1L, cy - radius)
  y2 <- min(h,  cy + radius)

  patch <- gray[y1:y2, x1:x2, drop = FALSE]
  xs <- seq.int(x1, x2)
  ys <- seq.int(y1, y2)
  mask <- outer(ys, xs, function(y, x) (x - cx)^2 + (y - cy)^2 <= radius^2)
  vals <- patch[mask]
  if (length(vals) == 0) return(NA_real_)
  255 - mean(vals, na.rm = TRUE)
}

classify_bubble_row <- function(inks,
                                min_ink = 35,
                                min_gap = 12,
                                double_gap = 8) {
  inks <- sort(inks, decreasing = TRUE)
  if (length(inks) == 0 || !all(is.finite(inks))) {
    return(list(answer = "", uncertain = TRUE, note = "non-finite ink score"))
  }
  top <- inks[1]
  second <- if (length(inks) >= 2) inks[2] else 0
  spread <- max(inks) - min(inks)
  choice <- names(inks)[1]

  if (top < min_ink || spread < min_gap) {
    return(list(answer = "", uncertain = FALSE, note = "blank"))
  }
  if (second >= min_ink && (top - second) < double_gap) {
    return(list(answer = paste0(choice, "*"), uncertain = TRUE,
                note = sprintf("ambiguous row: top=%s %.1f, second=%s %.1f",
                               names(inks)[1], top, names(inks)[2], second)))
  }
  list(answer = choice, uncertain = FALSE, note = "")
}

# questions_on_page: which of the configured questions have their bubble row on
# the given page of the form. page_no = NA means "the whole form is one page",
# which is what a single-page quiz and any pre-multi-page layout expect.
questions_on_page <- function(cfg, layout, page_no) {
  if (is.na(page_no)) return(cfg$questions)
  qp <- layout$QUESTION_PAGE
  keep <- vapply(cfg$questions, function(q) {
    pg <- qp[as.character(q)]
    !is.na(pg) && as.integer(pg) == as.integer(page_no)
  }, logical(1))
  cfg$questions[keep]
}

# Darkest ink within a small window around a bubble center. The window absorbs
# small registration error between the blank form and this particular scan.
bubble_darkness <- function(ctx, x_pos, y_pos) {
  radius <- max(5L, as.integer(ctx$w * 0.0062))
  search_offsets <- seq(-18L, 18L, by = 6L)
  pt <- ctx$map_xy(x_pos, y_pos)
  vals <- c()
  for (dx in search_offsets) {
    for (dy in search_offsets) {
      vals <- c(vals, ink_score(ctx$gray,
                                as.integer(pt["x"] + dx),
                                as.integer(pt["y"] + dy),
                                radius))
    }
  }
  max(vals, na.rm = TRUE)
}

# Unfilled ink for every bubble on the blank form, measured the same way a scan
# is read. The printed option letter is dark ink inside the circle, so an empty
# bubble is not zero, and that floor differs by letter (B is denser than A).
form_baseline <- function(layout, cfg) {
  if (is.null(layout$form_pdf) || !file.exists(layout$form_pdf)) {
    stop("Blank form not found for this layout (", layout$form_pdf %||% "no FORM_PDF",
         "). Re-run calibrate_coords() with the blank form PDF.", call. = FALSE)
  }
  n_pages <- pdftools::pdf_length(layout$form_pdf)
  pngs <- file.path(tempdir(), sprintf("bq-baseline-p%02d.png", seq_len(n_pages)))
  pdftools::pdf_convert(layout$form_pdf, format = "png", dpi = 150,
                        pages = seq_len(n_pages), filenames = pngs, verbose = FALSE)

  answers <- list()
  for (pg in seq_len(n_pages)) {
    qs <- questions_on_page(cfg, layout, pg)
    if (length(qs) == 0) next
    ctx <- build_map_xy(pngs[pg])
    for (q in qs) {
      q_chr <- as.character(q)
      col   <- layout$QUESTION_COL[q_chr]
      y_pos <- layout$QUESTION_Y[q_chr]
      answers[[q_chr]] <- vapply(cfg$options, function(letter) {
        bubble_darkness(ctx, layout$ANSWER_X[[col]][letter], y_pos)
      }, numeric(1))
    }
  }

  # zID baseline: rows are digits 0-9 (row d+1), columns are ID digit positions.
  zid <- NULL
  grid <- load_id_grid_from_form(layout$form_pdf, cfg)
  if (!is.null(grid)) {
    ctx1 <- build_map_xy(pngs[1])
    radius <- max(4L, as.integer(ctx1$w * 0.0042))
    zid <- vapply(seq_len(cfg$id$digits), function(col_i) {
      vapply(as.character(0:9), function(d) {
        pt <- ctx1$map_xy(grid$x[[as.character(col_i)]], grid$y[[d]])
        ink_score(ctx1$gray, as.integer(pt["x"]), as.integer(pt["y"]), radius)
      }, numeric(1))
    }, numeric(10))
  }
  width <- magick::image_info(magick::image_read(pngs[1]))$width
  list(answers = answers, zid = zid, width = width)
}

read_answers_cv <- function(img_path, cfg, layout, page_no = NA_integer_, baseline = NULL) {
  ctx <- build_map_xy(img_path)
  answers <- stats::setNames(vector("list", length(cfg$questions)),
                             as.character(cfg$questions))
  notes <- character(0)
  inks_by_q <- list()

  # A question whose bubbles are printed on another page is left as NA, which
  # is different from "" (printed here, left blank). Aggregation across the
  # sheet fills NA from the page that does carry the row; "" stays unanswered.
  this_page <- questions_on_page(cfg, layout, page_no)
  for (q in setdiff(cfg$questions, this_page)) {
    answers[[as.character(q)]] <- NA_character_
  }

  for (q in this_page) {
    q_chr <- as.character(q)
    col <- layout$QUESTION_COL[q_chr]
    y_pos <- layout$QUESTION_Y[q_chr]
    if (is.null(col) || is.na(col) || is.null(y_pos) || is.na(y_pos)) {
      answers[[q_chr]] <- ""
      notes <- c(notes, sprintf("Q%s missing layout coordinate", q_chr))
      next
    }

    inks <- vapply(cfg$options, function(letter) {
      x_pos <- layout$ANSWER_X[[col]][letter]
      if (is.null(x_pos) || is.na(x_pos)) return(NA_real_)
      bubble_darkness(ctx, x_pos, y_pos)
    }, numeric(1))
    names(inks) <- cfg$options
    if (!is.null(baseline)) inks <- inks - baseline[[q_chr]][cfg$options]
    inks_by_q[[q_chr]] <- inks

    cls <- classify_bubble_row(inks)
    answers[[q_chr]] <- cls$answer
    if (nzchar(cls$note) && cls$note != "blank") {
      notes <- c(notes, sprintf("Q%s %s", q_chr, cls$note))
    }
  }

  list(answers = answers, notes = notes, inks = inks_by_q, marker_ok = ctx$marker_ok)
}

load_id_grid_from_form <- function(form_pdf, cfg) {
  if (!file.exists(form_pdf)) return(NULL)
  txt <- pdftools::pdf_data(form_pdf, font_info = TRUE)[[1]]
  txt$xc <- (txt$x + txt$width / 2) / PAGE_W
  txt$yc <- (txt$y + txt$height / 2) / PAGE_H

  # The digit grid prints cfg$id$digits * 10 digits at one shared font size;
  # anything else with a lone digit (a date, "Page 1 of 2") is vastly
  # outnumbered, so the most common font size among digit text is the grid's,
  # whatever its absolute point size happens to be for this template.
  digit_candidates <- txt[txt$text %in% as.character(0:9), , drop = FALSE]
  if (nrow(digit_candidates) == 0) return(NULL)
  size_counts <- table(round(digit_candidates$font_size, 1))
  grid_size   <- as.numeric(names(size_counts)[which.max(size_counts)])
  digits <- digit_candidates[abs(digit_candidates$font_size - grid_size) < 0.5, , drop = FALSE]
  if (nrow(digits) < cfg$id$digits * 10) return(NULL)

  x_centers <- sort(unique(round(digits$xc, 4)))
  y_centers <- sort(unique(round(digits$yc, 4)))
  if (length(x_centers) < cfg$id$digits || length(y_centers) < 10) return(NULL)
  x_centers <- x_centers[seq_len(cfg$id$digits)]
  y_centers <- y_centers[seq_len(10)]
  list(
    x = stats::setNames(x_centers, seq_len(cfg$id$digits)),
    y = stats::setNames(y_centers, as.character(0:9))
  )
}

read_zid_cv <- function(img_path, cfg, form_pdf, baseline = NULL) {
  grid <- load_id_grid_from_form(form_pdf, cfg)
  if (is.null(grid)) {
    return(list(zid = NA_character_, uncertain = TRUE,
                note = "zID grid coordinates unavailable"))
  }
  ctx <- build_map_xy(img_path)
  radius <- max(4L, as.integer(ctx$w * 0.0042))

  digits <- character(cfg$id$digits)
  notes <- character(0)
  for (col_i in seq_len(cfg$id$digits)) {
    inks <- vapply(names(grid$y), function(digit) {
      pt <- ctx$map_xy(grid$x[[as.character(col_i)]], grid$y[[digit]])
      ink_score(ctx$gray, as.integer(pt["x"]), as.integer(pt["y"]), radius)
    }, numeric(1))
    names(inks) <- names(grid$y)
    if (!is.null(baseline)) inks <- inks - baseline[as.integer(names(grid$y)) + 1L, col_i]
    cls <- classify_bubble_row(inks, min_ink = 28, min_gap = 8, double_gap = 6)
    if (!nzchar(cls$answer) || grepl("\\*$", cls$answer)) {
      digits[col_i] <- "?"
      notes <- c(notes, sprintf("zID digit %d %s", col_i, cls$note))
    } else {
      digits[col_i] <- cls$answer
    }
  }
  zid <- paste0(cfg$id$prefix, paste(digits, collapse = ""))
  list(zid = zid, uncertain = any(digits == "?"), note = paste(notes, collapse = " | "))
}

image_crop_norm <- function(img, x1, y1, x2, y2) {
  info <- magick::image_info(img)
  left <- max(1L, as.integer(x1 * info$width))
  top <- max(1L, as.integer(y1 * info$height))
  right <- min(info$width, as.integer(x2 * info$width))
  bottom <- min(info$height, as.integer(y2 * info$height))
  magick::image_crop(img, sprintf("%dx%d+%d+%d",
                                  max(1L, right - left),
                                  max(1L, bottom - top),
                                  left, top))
}

image_crop_scan_uv <- function(img, map_xy, x1, y1, x2, y2) {
  info <- magick::image_info(img)
  pts <- rbind(map_xy(x1, y1), map_xy(x2, y1), map_xy(x1, y2), map_xy(x2, y2))
  left <- max(1L, as.integer(min(pts[, "x"], na.rm = TRUE)))
  top <- max(1L, as.integer(min(pts[, "y"], na.rm = TRUE)))
  right <- min(info$width, as.integer(max(pts[, "x"], na.rm = TRUE)))
  bottom <- min(info$height, as.integer(max(pts[, "y"], na.rm = TRUE)))
  magick::image_crop(img, sprintf("%dx%d+%d+%d",
                                  max(1L, right - left),
                                  max(1L, bottom - top),
                                  left, top))
}

compare_crop <- function(a, b) {
  prep <- function(img) {
    img <- magick::image_convert(img, colorspace = "gray")
    img <- magick::image_resize(img, "120x45!")
    arr <- magick::image_data(img, channels = "gray")
    as.numeric(suppressWarnings(strtoi(arr[1, , ], base = 16L))) / 255
  }
  va <- prep(a)
  vb <- prep(b)
  mean(abs(va - vb), na.rm = TRUE)
}

read_version_cv <- function(img_path, cfg, form_pdf) {
  pattern   <- sub("_v[0-9]+\\.pdf$", "_v%s.pdf", basename(form_pdf))
  templates <- file.path(dirname(form_pdf), sprintf(pattern, cfg$valid_versions))
  names(templates) <- cfg$valid_versions
  templates <- templates[file.exists(templates)]
  if (length(templates) == 0) {
    return(list(version = NA_character_, uncertain = TRUE,
                note = "version templates unavailable"))
  }

  ctx <- build_map_xy(img_path)
  # Tight crop around the printed version digit in the header.
  crop_box <- c(x1 = 0.865, y1 = 0.024, x2 = 0.925, y2 = 0.055)
  scan_crop <- image_crop_scan_uv(ctx$img, ctx$map_xy,
                                  crop_box["x1"], crop_box["y1"],
                                  crop_box["x2"], crop_box["y2"])
  scores <- vapply(templates, function(pdf) {
    tmpl <- magick::image_read_pdf(pdf, density = 150)[1]
    tmpl_crop <- image_crop_norm(tmpl, crop_box["x1"], crop_box["y1"],
                                 crop_box["x2"], crop_box["y2"])
    compare_crop(scan_crop, tmpl_crop)
  }, numeric(1))
  ord <- order(scores)
  best <- names(scores)[ord[1]]
  gap <- if (length(ord) > 1) scores[ord[2]] - scores[ord[1]] else Inf
  list(
    version = best,
    uncertain = is.finite(gap) && gap < 0.015,
    note = sprintf("version template scores: %s",
                   paste(sprintf("v%s=%.3f", names(scores), scores), collapse = ", "))
  )
}

decode_qr_cv <- function(img_path) {
  if (Sys.which("zbarimg") == "") {
    return(list(payload = NA_character_, note = "zbarimg not installed"))
  }
  # Use the same retry-on-upscale path the scan checker uses, so a page that
  # validates in check_scan_sequence() cannot then fail to decode here.
  found <- tryCatch(decode_qr_all(img_path), error = function(e) character(0))
  if (length(found) > 0) {
    return(list(payload = found[[1]], note = ""))
  }
  out <- tryCatch(
    system2("zbarimg", c("--quiet", "--raw", img_path), stdout = TRUE, stderr = TRUE),
    warning = function(w) structure(character(0), status = 1L),
    error = function(e) structure(character(0), status = 1L)
  )
  status <- attr(out, "status") %||% 0L
  if (!identical(status, 0L) || length(out) == 0 || !nzchar(out[1])) {
    return(list(payload = NA_character_, note = "QR not decoded"))
  }
  list(payload = out[1], note = "")
}

parse_qr_payload <- function(payload) {
  if (is.na(payload) || !nzchar(payload)) return(list())
  parts <- strsplit(payload, "[;|]", perl = TRUE)[[1]]
  kv <- parts[grepl("=", parts, fixed = TRUE)]
  out <- list()
  for (item in kv) {
    split <- strsplit(item, "=", fixed = TRUE)[[1]]
    out[[split[1]]] <- paste(split[-1], collapse = "=")
  }
  out
}

cv_parse_page <- function(img_path, page_num, cfg, layout, sheet_page = NA_integer_,
                          baseline = NULL) {
  qr <- decode_qr_cv(img_path)
  qr_fields <- parse_qr_payload(qr$payload)
  # Prefer the sheet page passed in (from the validated scan sequence); fall
  # back to what this page's own QR says.
  if (is.na(sheet_page) && !is.null(qr_fields$page)) {
    sheet_page <- suppressWarnings(as.integer(qr_fields$page))
  }
  read <- read_answers_cv(img_path, cfg, layout, sheet_page, baseline$answers)
  # The zID grid is printed on the front of the sheet only.
  zid <- if (is.na(sheet_page) || sheet_page == 1L) {
    read_zid_cv(img_path, cfg, layout$form_pdf, baseline$zid)
  } else {
    list(zid = NA_character_, uncertain = FALSE, note = "")
  }
  version <- if (!is.null(qr_fields$version) && qr_fields$version %in% cfg$valid_versions) {
    list(version = qr_fields$version, uncertain = FALSE, note = paste("QR:", qr$payload))
  } else {
    fallback <- read_version_cv(img_path, cfg, layout$form_pdf)
    fallback$note <- paste(c(qr$note, fallback$note), collapse = " | ")
    fallback
  }
  answers <- read$answers
  uncertain <- any(grepl("\\*$", unlist(answers), perl = TRUE))
  notes <- read$notes
  if (!isTRUE(read$marker_ok)) notes <- c(notes, "registration marker detection failed")
  if (isTRUE(zid$uncertain) && nzchar(zid$note)) notes <- c(notes, zid$note)
  if (isTRUE(version$uncertain)) notes <- c(notes, version$note)

  list(
    page         = page_num,
    sheet_page   = sheet_page,
    ok           = TRUE,
    zid          = zid$zid,
    name         = NA_character_,
    exam_version = version$version,
    answers      = answers,
    confidence   = if (uncertain || isTRUE(zid$uncertain) || isTRUE(version$uncertain)) "medium" else "high",
    notes        = paste(notes, collapse = " | "),
    error        = NA_character_
  )
}

#' Mark scanned forms using deterministic computer vision
#'
#' Reads calibrated answer bubbles directly from scan pixels. This currently
#' records answer bubbles and flags the row for review because version and zID
#' bubbles are not yet calibrated/read by the CV path.
#'
#' @param dir Folder created by [preprocess_scans()].
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param layout Path to the calibrated layout file, or a loaded layout list.
#' @param dry_run Process only the first 3 pending pages.
#' @return Invisibly, the updated progress data frame.
#' @export
mark_scans_cv <- function(dir,
                          config = default_config_path(),
                          layout = "output/layout.R",
                          dry_run = FALSE) {
  if (!dir.exists(dir)) stop("Directory not found: ", dir, call. = FALSE)
  cfg <- if (is.list(config)) config else load_exam_config(config)
  layout <- if (is.list(layout)) layout else load_layout(layout)

  csv_path <- file.path(dir, "progress.csv")
  if (!file.exists(csv_path)) {
    stop("progress.csv not found in ", dir,
         " -- run `bubblequiz preprocess` first.", call. = FALSE)
  }
  marked_dir <- file.path(dir, "marked-cv")
  dir.create(marked_dir, showWarnings = FALSE)

  message("Loading progress CSV: ", csv_path)
  progress <- readr::read_csv(csv_path, show_col_types = FALSE)
  baseline <- form_baseline(layout, cfg)
  if (nrow(progress) > 0) {
    scan_w <- magick::image_info(magick::image_read(file.path(dir, progress$file[1])))$width
    if (scan_w != baseline$width) {
      warning("Scans are ", scan_w, " px wide but the blank form baseline is ",
              baseline$width, " px. Rescan at the preprocess DPI (150) or the blank ",
              "baseline will not match.", call. = FALSE)
    }
  }
  if (!("name" %in% names(progress))) progress$name <- NA_character_

  # Sheet grouping from check_scan_sequence(), when it has been run. Without it
  # every page is treated as a whole sheet, which is correct for a single-page
  # form and detectably wrong for a multi-page one.
  seq_path <- file.path(dir, "scan_sequence.csv")
  if (file.exists(seq_path)) {
    seq_df <- readr::read_csv(seq_path, show_col_types = FALSE)
    m <- match(basename(progress$file), seq_df$file)
    progress$sheet      <- seq_df$sheet[m]
    progress$sheet_page <- seq_df$page_no[m]
    n_broken <- sum(is.na(progress$sheet))
    message("Using scan sequence: ", length(unique(stats::na.omit(progress$sheet))),
            " sheet(s)", if (n_broken) sprintf(", %d page(s) not in a complete sheet", n_broken) else "")
    if (n_broken > 0) {
      warning(n_broken, " page(s) are not part of a complete sheet and cannot be ",
              "attributed to a student. See ", seq_path, call. = FALSE)
    }
  } else {
    if (!("sheet" %in% names(progress))) progress$sheet <- NA_integer_
    if (!("sheet_page" %in% names(progress))) progress$sheet_page <- NA_integer_
    n_form_pages <- length(unique(stats::na.omit(as.integer(layout$QUESTION_PAGE))))
    if (n_form_pages > 1) {
      warning("This form has ", n_form_pages, " pages but no scan_sequence.csv was found. ",
              "Run check_scan_sequence() first, or page 2 answers cannot be attributed.",
              call. = FALSE)
    }
  }

  to_process <- seq_len(nrow(progress))
  if (isTRUE(dry_run)) {
    message("-- dry run: processing the first 3 pages only --")
    to_process <- utils::head(to_process, 3)
  }

  for (idx in to_process) {
    page_num <- progress$page[idx]
    img_path <- file.path(dir, progress$file[idx])
    message(sprintf("  Page %d / %d  [%s] ...", page_num, nrow(progress), progress$file[idx]))

    parsed <- tryCatch(
      cv_parse_page(img_path, page_num, cfg, layout, progress$sheet_page[idx], baseline),
      error = function(e) list(
        page = page_num, ok = FALSE, zid = NA_character_, name = NA_character_,
        exam_version = NA_character_, answers = NULL, confidence = NA_character_,
        notes = "", error = conditionMessage(e)
      )
    )
    progress <- update_progress_row(progress, idx, parsed, cfg, api_call_ok = NA)
    progress$needs_review[idx] <- TRUE
    if (isTRUE(parsed$ok)) {
      ans_line <- paste(sprintf("Q%s=%s", names(parsed$answers),
                                ifelse(nzchar(unlist(parsed$answers)),
                                       unlist(parsed$answers), "<blank>")),
                        collapse = "  ")
      message("    ", ans_line)
      message("    Notes: ", parsed$notes)
    } else {
      message("    CV error: ", parsed$error)
    }
    save_progress(progress, idx, csv_path)
    tryCatch(
      annotate_page(img_path, parsed, marked_dir, cfg, layout),
      error = function(e) message("    [annotate] failed: ", conditionMessage(e))
    )
  }

  cat("\n========================================\n")
  cat(sprintf("Pages processed by CV: %d\n", length(to_process)))
  cat("Rows needing QR/version/zID/ambiguous-answer review are flagged in progress.csv.\n")
  cat("========================================\n")
  invisible(progress)
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

