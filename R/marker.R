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
  out <- list(ANSWER_X = e$ANSWER_X, QUESTION_COL = e$QUESTION_COL,
              QUESTION_Y = e$QUESTION_Y, QUESTION_PAGE = question_page)
  attr(out, "path") <- normalizePath(path, mustWork = TRUE)
  out
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
# detect_corner_markers: locate the four printed corner squares in scan-space.
# Returns matrix with rows tl/tr/bl/br and cols x/y, or NULL.
#
# Each marker is a solid 5 mm square. It is found as the place where a box a
# little smaller than the marker is almost entirely dark -- something only a
# solid square satisfies; text, the QR code and the black "Version" banner all
# have white inside them at that scale. The centre is then the centroid of the
# dark pixels of that square alone. (Taking the median of all dark pixels near
# the expected corner, as this once did, let the Version banner beside the
# top-left marker pull the fit sideways by a bubble's width.)
# ---------------------------------------------------------------------------
MARKER_SIZE_MM <- 5

detect_corner_markers <- function(gray, w, h, fit_tol_mm = 2) {
  px_per_mm <- w / 210
  side <- MARKER_SIZE_MM * px_per_mm
  box <- max(3L, as.integer(round(side * 0.7)))
  dark_level <- 110

  find_one <- function(ref_xy) {
    cx <- ref_xy["x"] * w
    cy <- ref_xy["y"] * h
    # Search generously: scanners routinely shift the page by several mm, and a
    # skewed sheet moves its corners further. Solid-square detection and the
    # consistency check below keep a wide window from picking up anything else.
    r <- as.integer(25 * px_per_mm)
    x1 <- max(1L, as.integer(cx) - r); x2 <- min(w, as.integer(cx) + r)
    y1 <- max(1L, as.integer(cy) - r); y2 <- min(h, as.integer(cy) + r)
    dark <- gray[y1:y2, x1:x2, drop = FALSE] <= dark_level
    nr <- nrow(dark); nc <- ncol(dark)
    if (nr <= box || nc <= box) return(c(x = NA_real_, y = NA_real_))

    # Box sums from an integral image: fraction of dark pixels in every
    # box-by-box window, indexed by the window's top-left corner.
    ii <- matrix(0, nr + 1, nc + 1)
    ii[-1, -1] <- t(apply(apply(dark, 2, cumsum), 1, cumsum))
    rs <- seq_len(nr - box + 1); cs <- seq_len(nc - box + 1)
    frac <- (ii[rs + box, cs + box] - ii[rs, cs + box] -
               ii[rs + box, cs] + ii[rs, cs]) / box^2
    hits <- which(frac >= 0.9, arr.ind = TRUE)
    if (nrow(hits) == 0) return(c(x = NA_real_, y = NA_real_))

    # Of the solid regions, the marker is the one nearest where it was printed.
    hx <- x1 + hits[, "col"] - 1 + box / 2
    hy <- y1 + hits[, "row"] - 1 + box / 2
    k <- which.min((hx - cx)^2 + (hy - cy)^2)

    # Refine within one marker-width of the hit.
    half <- as.integer(ceiling(side * 0.75))
    wx1 <- max(1L, as.integer(hx[k]) - half); wx2 <- min(w, as.integer(hx[k]) + half)
    wy1 <- max(1L, as.integer(hy[k]) - half); wy2 <- min(h, as.integer(hy[k]) + half)
    win <- gray[wy1:wy2, wx1:wx2, drop = FALSE] <= dark_level
    # A marker touching the edge of the scan is cut off, however much of it
    # is left: its centre cannot be found.
    if ((wx1 <= 2 && any(win[, 1])) || (wy1 <= 2 && any(win[1, ])) ||
        (wx2 >= w - 1 && any(win[, ncol(win)])) || (wy2 >= h - 1 && any(win[nrow(win), ]))) {
      return(c(x = NA_real_, y = NA_real_))
    }
    # Measure the solid core: the rows and columns that are at least half dark
    # across a marker's width. A pen stroke touching the marker is too thin to
    # count, so it neither stretches the square nor moves its centre.
    rows <- which(rowSums(win) >= 0.5 * side)
    cols <- which(colSums(win) >= 0.5 * side)
    if (!length(rows) || !length(cols)) return(c(x = NA_real_, y = NA_real_))
    # A marker is an isolated square with ~1.25 mm of paper around it inside
    # this window. The end of a black banner or a block of print runs on to the
    # window's edge.
    if (min(rows) <= 1 || max(rows) >= nrow(win) || min(cols) <= 1 || max(cols) >= ncol(win)) {
      return(c(x = NA_real_, y = NA_real_))
    }
    ext <- c(diff(range(cols)), diff(range(rows))) + 1
    if (any(ext < 0.8 * side | ext > 1.35 * side)) return(c(x = NA_real_, y = NA_real_))
    core <- win[rows, cols, drop = FALSE]
    if (mean(core) < 0.85) return(c(x = NA_real_, y = NA_real_))
    cxm <- wx1 - 1 + mean(range(cols)); cym <- wy1 - 1 + mean(range(rows))
    c(x = cxm, y = cym)
  }

  pts <- t(vapply(seq_len(nrow(REG_MARKERS_REF)), function(i) {
    find_one(REG_MARKERS_REF[i, ])
  }, numeric(2)))

  rownames(pts) <- rownames(REG_MARKERS_REF)
  colnames(pts) <- c("x", "y")

  # The markers found must agree on one placement of the page. Printers and
  # scanners stretch a page by slightly different amounts across and down
  # (~96.9% x 96.0% on the Apeos); real four-marker pages agree to ~1 mm.
  ok <- stats::complete.cases(pts)
  src_mm <- cbind(REG_MARKERS_REF[, "x"] * 210, REG_MARKERS_REF[, "y"] * 297)
  px_mm <- w / 210
  # Leave-one-out: predict each marker from the others (affine from three,
  # or shift-rotation-scale from two) and drop the one that disagrees most,
  # while it disagrees by more than the tolerance. A least-squares fit of all
  # of them would spread one bad marker's error over the good ones.
  predict_from <- function(i, others) {
    if (length(others) >= 3) {
      M <- cbind(1, src_mm[others, , drop = FALSE])
      coef <- qr.solve(M, pts[others, , drop = FALSE])
      as.numeric(cbind(1, src_mm[i, , drop = FALSE]) %*% coef)
    } else {
      zr <- complex(real = src_mm[others, 1], imaginary = src_mm[others, 2])
      zs <- complex(real = pts[others, 1], imaginary = pts[others, 2])
      a <- (zs[2] - zs[1]) / (zr[2] - zr[1]); b <- zs[1] - a * zr[1]
      z <- a * complex(real = src_mm[i, 1], imaginary = src_mm[i, 2]) + b
      c(Re(z), Im(z))
    }
  }
  # Three markers fit an affine exactly, so instead of prediction they are held
  # to describing a plausible page: horizontal and vertical stretch within 3%
  # of each other, and under a degree of shear. A banner or print block taken
  # for a marker distorts the fit far beyond that.
  plausible3 <- function(i) {
    M <- cbind(1, src_mm[i, , drop = FALSE])
    J <- qr.solve(M, pts[i, , drop = FALSE])[2:3, , drop = FALSE]   # rows: d/dx_mm, d/dy_mm
    sx <- sqrt(sum(J[1, ]^2)); sy <- sqrt(sum(J[2, ]^2))
    shear <- abs(asin(sum(J[1, ] * J[2, ]) / (sx * sy))) * 180 / pi
    abs(sx / sy - 1) < 0.03 && shear < 1 && abs(sx / px_mm - 1) < 0.1
  }
  repeat {
    i <- which(ok)
    if (length(i) < 3) break
    loo <- vapply(i, function(k) {
      sqrt(sum((pts[k, ] - predict_from(k, setdiff(i, k)))^2))
    }, numeric(1))
    good <- if (length(i) == 4) max(loo) <= fit_tol_mm * px_mm else plausible3(i)
    if (good) break
    ok[i[which.max(loo)]] <- FALSE
  }
  zr_all <- complex(real = src_mm[, 1], imaginary = src_mm[, 2])
  zs_all <- complex(real = pts[, "x"], imaginary = pts[, "y"])
  if (sum(ok) == 2) {
    i <- which(ok)
    scale <- Mod(zs_all[i[2]] - zs_all[i[1]]) / (Mod(zr_all[i[2]] - zr_all[i[1]]) * px_mm)
    if (abs(scale - 1) > 0.08) ok[i] <- FALSE
  }
  pts[!ok, ] <- NA_real_
  if (sum(ok) < 2) return(NULL)
  pts
}

# fit_marker_map: map from template space (u, v) to scan pixels using however
# many corner markers were found. Four give a bilinear fit (the original);
# three an affine fit; two a similarity (shift, rotation, scale). A sheet fed
# crooked can lose a corner off the edge of the scan; with fewer than four the
# fit is rougher, and register_page() decides from the printed anchors whether
# the page can be read at all.
fit_marker_map <- function(pts) {
  ok <- stats::complete.cases(pts)
  src <- REG_MARKERS_REF[ok, , drop = FALSE]
  dst <- pts[ok, , drop = FALSE]
  if (sum(ok) == 4) return(fit_bilinear_map(src, dst))
  if (sum(ok) == 3) {
    M <- cbind(1, src[, "x"], src[, "y"])
    ax <- solve(M, dst[, "x"]); ay <- solve(M, dst[, "y"])
    return(map_result(function(u, v) {
      list(x = ax[1] + ax[2] * u + ax[3] * v, y = ay[1] + ay[2] * u + ay[3] * v)
    }))
  }
  # Two points: similarity in millimetre space, where the page is isotropic.
  zr <- complex(real = src[, "x"] * 210, imaginary = src[, "y"] * 297)
  zs <- complex(real = dst[, "x"], imaginary = dst[, "y"])
  a <- (zs[2] - zs[1]) / (zr[2] - zr[1])
  b <- zs[1] - a * zr[1]
  map_result(function(u, v) {
    z <- a * complex(real = u * 210, imaginary = v * 297) + b
    list(x = Re(z), y = Im(z))
  })
}

# ---------------------------------------------------------------------------
# fit_bilinear_map: fit bilinear map from template-space (u,v) to image pixels (x,y)
# x = ax0 + ax1*u + ax2*v + ax3*u*v ; same for y
# ---------------------------------------------------------------------------
fit_bilinear_map <- function(src_uv, dst_xy) {
  M <- cbind(1, src_uv[, "x"], src_uv[, "y"], src_uv[, "x"] * src_uv[, "y"])
  ax <- as.numeric(solve(M, dst_xy[, "x"]))
  ay <- as.numeric(solve(M, dst_xy[, "y"]))
  map_result(function(u, v) {
    list(x = ax[1] + ax[2] * u + ax[3] * v + ax[4] * u * v,
         y = ay[1] + ay[2] * u + ay[3] * v + ay[4] * u * v)
  })
}

# map_result: wrap a vectorised (u, v) -> list(x, y) mapping so that a single
# point returns c(x = , y = ) as the callers expect, and many points return a
# two-column matrix.
map_result <- function(f) {
  function(u, v) {
    r <- f(as.numeric(u), as.numeric(v))
    if (length(r$x) == 1) c(x = r$x, y = r$y) else cbind(x = r$x, y = r$y)
  }
}

# ---------------------------------------------------------------------------
# build_map_xy: build a coordinate mapping function from an image
# ---------------------------------------------------------------------------
build_map_xy <- function(img_path) {
  img  <- if (inherits(img_path, "magick-image")) img_path else magick::image_read(img_path)
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
      fit_marker_map(marker_pts)
    } else {
      map_result(function(u, v) list(x = u * w, y = v * h))
    },
    marker_ok  = !is.null(marker_pts),
    markers    = if (is.null(marker_pts)) 0L else sum(stats::complete.cases(marker_pts)),
    marker_pts = marker_pts
  )
}

# page_angle: rotation of the page in the scan, in degrees (clockwise
# positive), from the mapped direction of a horizontal line across it.
page_angle <- function(map_xy) {
  a <- map_xy(0.2, 0.5); b <- map_xy(0.8, 0.5)
  atan2(b[["y"]] - a[["y"]], b[["x"]] - a[["x"]]) * 180 / pi
}

# registration_anchors: printed text on a page that is never written over,
# used to check (and, when corner markers are lost, to fix) the registration:
# every "Answer Qn" label, and on the front the "Fill zID digit bubbles:"
# heading above the zID grid. One row per anchor: centre (u, v) and half-size
# (hu, hv), as page fractions.
registration_anchors <- function(cfg, layout, page_no) {
  qs <- questions_on_page(cfg, layout, page_no)
  rows <- lapply(qs, function(q) {
    col <- layout$QUESTION_COL[[as.character(q)]]
    x_a <- layout$ANSWER_X[[col]][[1]]
    # "Answer Qn" runs from ~66 pt to ~15 pt left of the first bubble's centre.
    c(u = x_a - 41 / PAGE_W, v = layout$QUESTION_Y[[as.character(q)]],
      hu = 27 / PAGE_W, hv = 9 / PAGE_H)
  })
  heading <- zid_heading_box(form_pdf_for(layout), if (is.na(page_no)) 1L else page_no)
  if (!is.null(heading)) rows <- c(rows, list(heading))
  if (!length(rows)) return(matrix(numeric(0), ncol = 4, dimnames = list(NULL, c("u", "v", "hu", "hv"))))
  do.call(rbind, rows)
}

# zid_heading_box: where "Fill zID digit bubbles:" is printed on a form page,
# from the PDF's text layer, or NULL if the page has no such heading.
zid_heading_box <- function(form_pdf, page) {
  if (is.null(form_pdf) || !file.exists(form_pdf)) return(NULL)
  txt <- pdftools::pdf_data(form_pdf)
  if (page > length(txt)) return(NULL)
  d <- txt[[page]]
  i <- which(d$text == "Fill")
  if (!length(i)) return(NULL)
  line <- d[abs(d$y - d$y[i[1]]) < 2 & d$x >= d$x[i[1]], , drop = FALSE]
  line <- line[order(line$x), , drop = FALSE]
  line <- line[seq_len(min(nrow(line), 4)), , drop = FALSE]
  x1 <- min(line$x); x2 <- max(line$x + line$width)
  y1 <- min(line$y); y2 <- max(line$y + line$height)
  c(u = (x1 + x2) / 2 / PAGE_W, v = (y1 + y2) / 2 / PAGE_H,
    hu = (x2 - x1) / 2 / PAGE_W, hv = ((y2 - y1) / 2 + 2) / PAGE_H)
}

# registration_offsets: how far the mapped page is from where it should be, at
# each anchor. The anchor area is sampled from the scan through the page
# mapping -- so a rotated or stretched page is compared in the form's own
# coordinates -- and slid against the same area of the blank form. The best
# matching shift is the registration error there, in scan pixels; NA where the
# anchor could not be located.
registration_offsets <- function(ctx, blank_ctx, anchors, search = 8L) {
  if (!nrow(anchors)) return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("dx", "dy"))))
  sample_gray <- function(g, xy) {
    r <- round(xy[, "y"]); c <- round(xy[, "x"])
    out <- rep(NA_real_, length(r))
    ok <- r >= 1 & r <= nrow(g) & c >= 1 & c <= ncol(g)
    out[ok] <- g[cbind(r[ok], c[ok])]
    out
  }
  step <- max(1L, as.integer(search %/% 6L))
  t(vapply(seq_len(nrow(anchors)), function(k) {
    a <- anchors[k, ]
    us <- seq(a[["u"]] - a[["hu"]], a[["u"]] + a[["hu"]], length.out = max(8L, as.integer(2 * a[["hu"]] * blank_ctx$w)))
    vs <- seq(a[["v"]] - a[["hv"]], a[["v"]] + a[["hv"]], length.out = max(8L, as.integer(2 * a[["hv"]] * blank_ctx$h)))
    uv <- expand.grid(u = us, v = vs)
    tmpl <- sample_gray(blank_ctx$gray, blank_ctx$map_xy(uv$u, uv$v))
    if (anyNA(tmpl) || stats::sd(tmpl) == 0) return(c(dx = NA_real_, dy = NA_real_))
    tmpl <- tmpl - mean(tmpl)
    at <- ctx$map_xy(uv$u, uv$v)
    score <- function(dx, dy) {
      p <- sample_gray(ctx$gray, at + matrix(c(dx, dy), nrow(at), 2, byrow = TRUE))
      if (anyNA(p)) return(-Inf)
      p <- p - mean(p)
      r <- sum(tmpl * p) / sqrt(sum(tmpl^2) * sum(p^2))
      if (is.finite(r)) r else -Inf
    }
    # Coarse grid over the whole window, then every pixel around the best.
    best <- -Inf; found <- c(0, 0)
    for (dy in seq(-search, search, by = step)) for (dx in seq(-search, search, by = step)) {
      r <- score(dx, dy); if (r > best) { best <- r; found <- c(dx, dy) }
    }
    if (step > 1L) {
      c0 <- found
      for (dy in (c0[2] - step):(c0[2] + step)) for (dx in (c0[1] - step):(c0[1] + step)) {
        r <- score(dx, dy); if (r > best) { best <- r; found <- c(dx, dy) }
      }
    }
    # An anchor that matches nowhere well is not located, rather than
    # "located" at whichever shift was least bad.
    if (!is.finite(best) || best < 0.5) return(c(dx = NA_real_, dy = NA_real_))
    c(dx = found[1], dy = found[2])
  }, numeric(2)))
}

# register_page: map a scanned page onto its form, and check the result
# against printed text (the anchors) before anything on it is read. Two or
# three usable corner markers are enough to try -- a clipped corner is common
# -- but the page only counts as registered if every anchor lands within
# tolerance. A sheet fed badly skewed or shifted fails here and is flagged to
# be rescanned straight; the marker does not try to recover it.
register_page <- function(img_path, cfg, layout, page_no) {
  ctx <- build_map_xy(img_path)
  blank <- blank_form_ctx(form_pdf_for(layout), page_no, ctx$w)
  anchors <- registration_anchors(cfg, layout, page_no)
  scale <- ctx$w / 1654
  off <- registration_offsets(ctx, blank, anchors, search = as.integer(round(8 * scale)))
  err <- if (!nrow(off)) 0 else if (anyNA(off)) Inf else max(abs(off))
  registered <- isTRUE(ctx$marker_ok) && err <= REGISTRATION_TOLERANCE_PX * scale
  note <- if (registered) "" else paste0(
    "page out of register (", if (is.finite(err)) sprintf("off by %.0f px", err) else "could not be located",
    ", ", ctx$markers, " of 4 corner markers usable): fed skewed or shifted -- ",
    "rescan it straight, or enter the answers by hand")
  list(ctx = ctx, blank = blank, registered = registered, error = err, note = note)
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

# Bubble reading thresholds, in units of bubble_score(): ink above the blank
# form. Measured on real scans (Sept 2026, 200 dpi, 11 hand-marked sheets):
# filled answer bubbles scored >= 34.8 and empty ones <= -0.5; filled zID
# bubbles >= 30.6 and empty ones <= 12.6. Each rule leaves a margin on both
# sides, and anything between the lines is flagged for a human, not guessed.
#   radius / search: disc size and registration slack, as fractions of width.
#   mark:  a bubble this far above the blank form is a mark.
#   blank: a row whose darkest bubble is below this is unanswered.
#   gap:   the chosen bubble must beat every other bubble in its row by this.
# A second bubble at or above `mark` is a second mark, and flags the row
# whatever the gap: on the full first session (81 sheets) that caught three
# corrections that a gap-only rule accepted wrongly.
ANSWER_READ <- list(radius = 0.0075, search = 0.0024, mark = 18, blank = 10, gap = 12)
ZID_READ    <- list(radius = 0.0055, search = 0.0012, mark = 22, blank = 16, gap = 12)

classify_bubble_row <- function(inks, rule) {
  # Checked before sorting: sort() silently drops NA, which would hide an
  # unreadable bubble and let the rest of the row look clean.
  if (length(inks) == 0 || !all(is.finite(inks))) {
    return(list(answer = "", uncertain = TRUE, note = "non-finite ink score"))
  }
  inks <- sort(inks, decreasing = TRUE)
  top <- inks[1]
  second <- if (length(inks) >= 2) inks[2] else 0
  choice <- names(inks)[1]

  if (top < rule$blank) {
    return(list(answer = "", uncertain = TRUE, note = "blank"))
  }
  if (top < rule$mark) {
    return(list(answer = paste0(choice, "*"), uncertain = TRUE,
                note = sprintf("faint mark: %s %.1f", choice, top)))
  }
  # Two marks in one row always go to a person, however far apart their
  # scores. A crossed-out bubble is a filled bubble with more ink on top, so it
  # usually scores darker than the answer the student meant -- on real scans
  # the darker of two marks was the crossed-out one as often as not.
  if (second >= rule$mark) {
    return(list(answer = paste0(choice, "*"), uncertain = TRUE,
                note = sprintf("two marks: %s %.1f and %s %.1f (one may be crossed out)",
                               choice, top, names(inks)[2], second)))
  }
  if (top - second < rule$gap) {
    return(list(answer = paste0(choice, "*"), uncertain = TRUE,
                note = sprintf("ambiguous row: top=%s %.1f, second=%s %.1f",
                               choice, top, names(inks)[2], second)))
  }
  list(answer = choice, uncertain = FALSE, note = "")
}

# bubble_score: how much darker the scan is than the blank form inside one
# bubble. Subtracting the blank form cancels the printed letter or digit, which
# otherwise dominates a faint mark: an empty "8" carries far more ink than an
# empty "1". The scan is sampled over a small window around the mapped centre
# to absorb registration error, and the median taken: a fill is dark wherever
# the disc lands, while a printed digit a pixel out of register is dark only at
# some offsets. (Taking the maximum instead picked the worst-aligned offset and
# pushed empty zID bubbles to within 10 points of real marks.) The blank form
# is rendered from the PDF and needs no search.
bubble_score <- function(ctx, blank_ctx, u, v, rule) {
  radius <- max(3L, as.integer(round(ctx$w * rule$radius)))
  step   <- max(1L, as.integer(round(ctx$w * rule$search / 2)))
  offs   <- seq(-2L * step, 2L * step, by = step)
  pt <- ctx$map_xy(u, v)
  scan_ink <- stats::median(unlist(lapply(offs, function(dx) vapply(offs, function(dy) {
    ink_score(ctx$gray, as.integer(pt["x"] + dx), as.integer(pt["y"] + dy), radius)
  }, numeric(1)))))
  bpt <- blank_ctx$map_xy(u, v)
  b_radius <- max(3L, as.integer(round(blank_ctx$w * rule$radius)))
  scan_ink - ink_score(blank_ctx$gray, as.integer(bpt["x"]), as.integer(bpt["y"]), b_radius)
}

# blank_form_ctx: the blank form page rendered at the scan's pixel width, with
# its own registration. Cached, since every scan of a version shares it.
.blank_cache <- new.env(parent = emptyenv())
blank_form_ctx <- function(form_pdf, page, width) {
  if (is.null(form_pdf) || !file.exists(form_pdf)) {
    stop("Blank form PDF not found: ", form_pdf %||% "<none>",
         "\nThe marker reads each bubble against the blank form; pass `forms` ",
         "or keep quizform_v<N>.pdf beside the layout file.", call. = FALSE)
  }
  page <- if (is.na(page)) 1L else as.integer(page)
  key <- paste(normalizePath(form_pdf), page, width, file.mtime(form_pdf), sep = "|")
  if (!is.null(.blank_cache[[key]])) return(.blank_cache[[key]])
  dpi <- width / (PAGE_W / 72)
  img <- magick::image_read_pdf(form_pdf, pages = page, density = dpi)
  img <- magick::image_resize(img, sprintf("%dx", width))
  ctx <- build_map_xy(magick::image_flatten(magick::image_background(img, "white")))
  if (!isTRUE(ctx$marker_ok)) {
    stop("Could not find the corner markers on the blank form ", form_pdf, call. = FALSE)
  }
  .blank_cache[[key]] <- ctx
  ctx
}

form_pdf_for <- function(layout) {
  attr(layout, "form_pdf") %||%
    file.path(dirname(attr(layout, "path") %||% "output/layout.R"), "quizform_v1.pdf")
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

# Registration tolerance, in pixels at 200 dpi (scaled to the scan's width):
# how far printed text may sit from where the mapping puts it. On 324 real
# pages (Sept 2026), well-fed sheets measured at most 6 px (0.8 mm) -- print
# distortion, largest near the bottom -- which bubble reading absorbs (answer
# discs have ~10 px of room, zID discs ~8). The anchor search reaches 8 px, so
# an anchor found only at the edge of it, or not at all, fails the page.
REGISTRATION_TOLERANCE_PX <- 7

read_answers_cv <- function(img_path, cfg, layout, page_no = NA_integer_, reg = NULL) {
  if (is.null(reg)) reg <- register_page(img_path, cfg, layout, page_no)
  ctx <- reg$ctx
  blank <- reg$blank
  registered <- reg$registered
  reg_err <- reg$error
  reg_note <- reg$note
  answers <- stats::setNames(vector("list", length(cfg$questions)),
                             as.character(cfg$questions))
  notes <- character(0)
  inks_by_q <- list()
  uncertain <- FALSE

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
      uncertain <- TRUE
      notes <- c(notes, sprintf("Q%s missing layout coordinate", q_chr))
      next
    }

    inks <- vapply(cfg$options, function(letter) {
      x_pos <- layout$ANSWER_X[[col]][letter]
      if (is.null(x_pos) || is.na(x_pos)) return(NA_real_)
      bubble_score(ctx, blank, x_pos, y_pos, ANSWER_READ)
    }, numeric(1))
    names(inks) <- cfg$options
    inks_by_q[[q_chr]] <- inks

    cls <- classify_bubble_row(inks, ANSWER_READ)
    answers[[q_chr]] <- cls$answer
    uncertain <- uncertain || isTRUE(cls$uncertain)
    if (nzchar(cls$note)) notes <- c(notes, sprintf("Q%s %s", q_chr, cls$note))
  }

  if (!registered) {
    # Nothing read from a page out of register is reported as a letter: every
    # question printed on it becomes "*" (uncertain, no reading).
    for (q in this_page) answers[[as.character(q)]] <- "*"
    notes <- reg_note
    uncertain <- TRUE
  }

  list(answers = answers, notes = notes, inks = inks_by_q,
       uncertain = uncertain, marker_ok = registered, reg_error = reg_err)
}

load_id_grid_from_form <- function(form_pdf, cfg) {
  if (!file.exists(form_pdf)) return(NULL)
  txt <- pdftools::pdf_data(form_pdf, font_info = TRUE)[[1]]
  txt$xc <- (txt$x + txt$width / 2) / PAGE_W
  txt$yc <- (txt$y + txt$height / 2) / PAGE_H
  digits <- txt[txt$text %in% as.character(0:9) &
                  txt$font_size < 7, , drop = FALSE]
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

read_zid_cv <- function(img_path, cfg, form_pdf, ctx = NULL) {
  grid <- load_id_grid_from_form(form_pdf, cfg)
  if (is.null(grid)) {
    return(list(zid = NA_character_, uncertain = TRUE,
                note = "zID grid coordinates unavailable"))
  }
  if (is.null(ctx)) ctx <- build_map_xy(img_path)
  blank <- blank_form_ctx(form_pdf, 1L, ctx$w)

  digits <- character(cfg$id$digits)
  notes <- character(0)
  for (col_i in seq_len(cfg$id$digits)) {
    inks <- vapply(names(grid$y), function(digit) {
      bubble_score(ctx, blank, grid$x[[as.character(col_i)]], grid$y[[digit]], ZID_READ)
    }, numeric(1))
    names(inks) <- names(grid$y)
    cls <- classify_bubble_row(inks, ZID_READ)
    if (isTRUE(cls$uncertain)) {
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

read_version_cv <- function(img_path, cfg, template_dir) {
  templates <- file.path(template_dir, sprintf("quizform_v%s.pdf", cfg$valid_versions))
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

cv_parse_page <- function(img_path, page_num, cfg, layouts, sheet_page = NA_integer_,
                          seq_version = NA_character_, seq_status = NA_character_) {
  qr <- decode_qr_cv(img_path)
  qr_fields <- parse_qr_payload(qr$payload)
  # Prefer the sheet page passed in (from the validated scan sequence); fall
  # back to what this page's own QR says.
  if (is.na(sheet_page) && !is.null(qr_fields$page)) {
    sheet_page <- suppressWarnings(as.integer(qr_fields$page))
  }

  # The version decides which layout to read with, so it comes first. Versions
  # shuffle the options, which changes line breaks and moves the answer rows by
  # a few points; each version is read against its own calibrated form.
  per_version <- is_layout_set(layouts)
  version <- if (!is.null(qr_fields$version) && qr_fields$version %in% cfg$valid_versions) {
    list(version = qr_fields$version, uncertain = FALSE, note = "")
  } else if (!is.na(seq_version) && seq_version %in% cfg$valid_versions) {
    # The sequence check identified this page from the other side of its sheet.
    list(version = seq_version, uncertain = TRUE,
         note = "version taken from the other side of the sheet")
  } else {
    list(version = NA_character_, uncertain = TRUE,
         note = paste(c(qr$note, "version unknown: page QR unreadable"), collapse = " | "))
  }
  layout <- if (!per_version) {
    layouts
  } else if (!is.na(version$version) && !is.null(layouts[[version$version]])) {
    layouts[[version$version]]
  } else {
    layouts[[1]]
  }

  reg <- register_page(img_path, cfg, layout, sheet_page)
  read <- read_answers_cv(img_path, cfg, layout, sheet_page, reg = reg)
  form_pdf <- form_pdf_for(layout)
  # The zID grid is printed on the front of the sheet only.
  on_front <- is.na(sheet_page) || sheet_page == 1L
  zid <- if (on_front && !isTRUE(read$marker_ok)) {
    list(zid = paste0(cfg$id$prefix, strrep("?", cfg$id$digits)), uncertain = TRUE, note = "")
  } else if (on_front) {
    read_zid_cv(img_path, cfg, form_pdf, ctx = reg$ctx)
  } else {
    list(zid = NA_character_, uncertain = FALSE, note = "")
  }

  answers <- read$answers
  uncertain <- isTRUE(read$uncertain)
  notes <- read$notes

  if (isTRUE(zid$uncertain) && nzchar(zid$note)) notes <- c(notes, zid$note)
  if (isTRUE(version$uncertain)) notes <- c(notes, version$note)

  # A page is only trusted when every part of it was read cleanly. Anything
  # less goes to a human rather than into the gradebook.
  # A sheet the sequence check had to recover (scanned back first, or a side
  # identified from the other) is marked, but a person confirms it.
  recovered <- !is.na(seq_status) && startsWith(seq_status, "ok:")
  if (recovered) notes <- c(notes, sub("^ok: ", "", seq_status))
  needs_review <- uncertain || isTRUE(zid$uncertain) || isTRUE(version$uncertain) ||
    !isTRUE(read$marker_ok) || (on_front && is.na(zid$zid)) || recovered

  list(
    page         = page_num,
    sheet_page   = sheet_page,
    ok           = TRUE,
    zid          = zid$zid,
    name         = NA_character_,
    exam_version = version$version,
    answers      = answers,
    confidence   = if (needs_review) "medium" else "high",
    needs_review = needs_review,
    notes        = paste(notes, collapse = " | "),
    error        = NA_character_,
    layout       = layout
  )
}

# A layout set is a named list of per-version layouts; a single layout is the
# list load_layout() returns.
is_layout_set <- function(x) is.list(x) && !("ANSWER_X" %in% names(x))

#' Calibrate every version's form at marking time
#'
#' Reads the bubble positions for each exam version straight from its
#' `quizform_v<N>.pdf`. Marking against the forms themselves means a layout
#' can never be older than the paper it is reading.
#'
#' @param cfg A loaded exam config.
#' @param forms_dir Folder holding `quizform_v<N>.pdf` for every version.
#' @return A named list of layouts, one per version.
#' @export
form_layouts <- function(cfg, forms_dir) {
  pdfs <- file.path(forms_dir, sprintf("quizform_v%s.pdf", cfg$valid_versions))
  missing <- pdfs[!file.exists(pdfs)]
  if (length(missing)) {
    stop("Form PDF(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- lapply(pdfs, function(pdf) {
    lay <- suppressMessages(layout_from_form(cfg, pdf))
    lay$n_rows <- NULL
    attr(lay, "form_pdf") <- normalizePath(pdf)
    lay
  })
  stats::setNames(out, cfg$valid_versions)
}

# Refuse a layout.R that is older than the forms it describes. Rebuilding the
# forms moves the bubbles, and a stale layout reads every answer from the
# wrong place without any other sign that something is wrong.
check_layout_fresh <- function(layout_path) {
  src <- grep("^# Source: ", readLines(layout_path, n = 5), value = TRUE)
  forms <- Sys.glob(file.path(dirname(layout_path), "quizform_v*.pdf"))
  if (length(src)) forms <- c(forms, sub("^# Source: ", "", src))
  forms <- forms[file.exists(forms)]
  newer <- forms[file.mtime(forms) > file.mtime(layout_path)]
  if (length(newer)) {
    stop("Layout file ", layout_path, " is older than ",
         paste(basename(unique(newer)), collapse = ", "),
         ".\nThe forms were rebuilt after calibration; re-run calibrate_coords(), ",
         "or pass `forms` to mark from the form PDFs directly.", call. = FALSE)
  }
  invisible(TRUE)
}

# resolve_layouts: decide what the marker reads bubbles against. A layout list
# passed in is used as given. Otherwise the form PDFs win whenever they are
# present -- `forms`, or the folder holding layout.R -- because they are the
# paper itself; layout.R is only a fallback, and a stale one is refused.
resolve_layouts <- function(cfg, layout, forms = NULL) {
  if (is.list(layout)) return(layout)
  forms_dir <- forms %||% dirname(layout)
  pdfs <- file.path(forms_dir, sprintf("quizform_v%s.pdf", cfg$valid_versions))
  if (all(file.exists(pdfs))) {
    message("Calibrating ", length(pdfs), " version(s) from the form PDFs in ", forms_dir)
    return(form_layouts(cfg, forms_dir))
  }
  if (!is.null(forms)) {
    stop("Form PDF(s) not found: ", paste(pdfs[!file.exists(pdfs)], collapse = ", "),
         call. = FALSE)
  }
  lay <- load_layout(layout)
  check_layout_fresh(layout)
  lay
}

#' Mark scanned forms using deterministic computer vision
#'
#' Reads answer and zID bubbles directly from scan pixels. Each page's version
#' comes from its QR code, and the page is read against that version's own
#' form. A page is flagged `needs_review` only when something on it was not
#' read cleanly: an ambiguous bubble, an unreadable zID digit, an unreadable
#' QR, or failed registration.
#'
#' @param dir Folder created by [preprocess_scans()].
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param layout Path to the calibrated layout file, or a loaded layout list
#'   (a single layout, or a per-version set from [form_layouts()]). When the
#'   folder holding the layout file also holds `quizform_v<N>.pdf` for every
#'   version, those forms are calibrated directly and the file is not used.
#' @param forms Folder holding `quizform_v<N>.pdf` for every version. Defaults
#'   to the folder holding `layout`.
#' @param dry_run Process only the first 3 pending pages.
#' @return Invisibly, the updated progress data frame.
#' @export
mark_scans_cv <- function(dir,
                          config = default_config_path(),
                          layout = "output/layout.R",
                          forms = NULL,
                          dry_run = FALSE) {
  if (!dir.exists(dir)) stop("Directory not found: ", dir, call. = FALSE)
  cfg <- if (is.list(config)) config else load_exam_config(config)
  layout <- resolve_layouts(cfg, layout, forms)

  csv_path <- file.path(dir, "progress.csv")
  if (!file.exists(csv_path)) {
    stop("progress.csv not found in ", dir,
         " -- run `bubblequiz preprocess` first.", call. = FALSE)
  }
  marked_dir <- file.path(dir, "marked-cv")
  dir.create(marked_dir, showWarnings = FALSE)

  message("Loading progress CSV: ", csv_path)
  progress <- readr::read_csv(csv_path, show_col_types = FALSE)
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
    seq_version <- as.character(seq_df$version[m])
    seq_status  <- as.character(seq_df$status[m])
    n_broken <- sum(is.na(progress$sheet))
    message("Using scan sequence: ", length(unique(stats::na.omit(progress$sheet))),
            " sheet(s)", if (n_broken) sprintf(", %d page(s) not in a complete sheet", n_broken) else "")
    if (n_broken > 0) {
      warning(n_broken, " page(s) are not part of a complete sheet and cannot be ",
              "attributed to a student. See ", seq_path, call. = FALSE)
    }
  } else {
    seq_version <- rep(NA_character_, nrow(progress))
    seq_status  <- rep(NA_character_, nrow(progress))
    if (!("sheet" %in% names(progress))) progress$sheet <- NA_integer_
    if (!("sheet_page" %in% names(progress))) progress$sheet_page <- NA_integer_
    first <- if (is_layout_set(layout)) layout[[1]] else layout
    n_form_pages <- length(unique(stats::na.omit(as.integer(first$QUESTION_PAGE))))
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
      cv_parse_page(img_path, page_num, cfg, layout, progress$sheet_page[idx],
                    seq_version = seq_version[idx], seq_status = seq_status[idx]),
      error = function(e) list(
        page = page_num, ok = FALSE, zid = NA_character_, name = NA_character_,
        exam_version = NA_character_, answers = NULL, confidence = NA_character_,
        notes = "", error = conditionMessage(e)
      )
    )
    progress <- update_progress_row(progress, idx, parsed, cfg, api_call_ok = NA)
    progress$needs_review[idx] <- !isTRUE(parsed$ok) || isTRUE(parsed$needs_review)
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
      annotate_page(img_path, parsed, marked_dir, cfg, parsed$layout),
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
