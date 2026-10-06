# Helpers for tests that run the marker on page images.

# Write JPEG page images into a PDF the way office scanners do: one full-page
# greyscale image per page, JPEG-compressed and, with `flate = TRUE`, deflated
# again on top (the Fujifilm Apeos filter chain [/FlateDecode /DCTDecode]).
# This is the format that pdftools renders as blank pages, so tests built on
# it exercise the same path as a real scan.
write_scan_pdf <- function(jpegs, out, flate = TRUE, colour = FALSE) {
  con <- file(out, "wb")
  on.exit(close(con))
  offs <- integer(0)
  pos <- 0
  w <- function(x) {
    if (is.character(x)) x <- charToRaw(x)
    writeBin(x, con)
    pos <<- pos + length(x)
  }
  obj <- function(n, ...) {
    offs[n] <<- pos
    w(sprintf("%d 0 obj\n", n))
    for (b in list(...)) w(b)
    w("\nendobj\n")
  }
  n <- length(jpegs)
  w("%PDF-1.3\n")
  kids <- paste(sprintf("%d 0 R", 3 + (seq_len(n) - 1) * 3), collapse = " ")
  obj(1, "<< /Type /Catalog /Pages 2 0 R >>")
  obj(2, sprintf("<< /Type /Pages /Kids [%s] /Count %d >>", kids, n))
  for (i in seq_len(n)) {
    info <- magick::image_info(magick::image_read(jpegs[i]))
    data <- readBin(jpegs[i], "raw", file.size(jpegs[i]))
    filt <- "/DCTDecode"
    if (flate) {
      data <- memCompress(data, "gzip")
      filt <- "[/FlateDecode /DCTDecode]"
    }
    base <- 3 + (i - 1) * 3
    cs <- sprintf("q\n595.44 0 0 842.40 0.00 0.00 cm\n/Im%d Do\nQ\n", i)
    obj(base, sprintf(paste0("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595.44 842.40] ",
                             "/Contents %d 0 R /Resources << /XObject << /Im%d %d 0 R >> >> >>"),
                      base + 1, i, base + 2))
    obj(base + 1, sprintf("<< /Length %d >>\nstream\n", nchar(cs)), cs, "\nendstream")
    obj(base + 2,
        sprintf(paste0("<< /Type /XObject /Subtype /Image /Width %d /Height %d ",
                       "/BitsPerComponent 8 /ColorSpace %s /Filter %s /Length %d >>\nstream\n"),
                info$width, info$height, if (colour) "/DeviceRGB" else "/DeviceGray",
                filt, length(data)),
        data, "\nendstream")
  }
  xref <- pos
  nobj <- 2 + 3 * n
  w(sprintf("xref\n0 %d\n0000000000 65535 f \n", nobj + 1))
  for (k in seq_len(nobj)) w(sprintf("%010d 00000 n \n", offs[k]))
  w(sprintf("trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n", nobj + 1, xref))
  invisible(out)
}

# Render a blank form page as a JPEG at scanner resolution, greyscale unless
# `colour = TRUE`.
form_page_jpeg <- function(form_pdf, page, out, dpi = 200, colour = FALSE) {
  img <- magick::image_read_pdf(form_pdf, pages = page, density = dpi)
  img <- magick::image_flatten(magick::image_background(img, "white"))
  img <- magick::image_convert(img, colorspace = if (colour) "sRGB" else "gray")
  magick::image_write(img, out, format = "jpeg", quality = 85)
  out
}

# Fill bubbles on a page image, given page-fraction centres (u, v) and a
# radius as a fraction of page width. `shade` is 0 (black) to 255 (white);
# `ink` overrides it with any R colour, and keeps the page in colour.
fill_bubbles <- function(img_path, uv, radius = 0.0075, shade = 40, ink = NULL) {
  img <- magick::image_read(img_path)
  info <- magick::image_info(img)
  img <- magick::image_draw(img)
  graphics::symbols(uv[, 1] * info$width, uv[, 2] * info$height,
                    circles = rep(radius * info$width, nrow(uv)), inches = FALSE,
                    add = TRUE, fg = NA,
                    bg = ink %||% grDevices::rgb(shade, shade, shade, maxColorValue = 255))
  grDevices::dev.off()
  if (is.null(ink)) img <- magick::image_convert(img, colorspace = "gray")
  magick::image_write(img, img_path, format = "jpeg", quality = 85)
  img_path
}

# Mimic a scanner: shift the page by (dx, dy) mm, rotate by `deg`, add blur.
scannerise <- function(img_path, dx_mm = 0, dy_mm = 0, deg = 0, blur = 0.6) {
  img <- magick::image_read(img_path)
  info <- magick::image_info(img)
  px <- info$width / 210
  if (deg != 0) {
    img <- magick::image_rotate(img, deg)
    img <- magick::image_crop(img, sprintf("%dx%d+%d+%d", info$width, info$height,
                                           as.integer((magick::image_info(img)$width - info$width) / 2),
                                           as.integer((magick::image_info(img)$height - info$height) / 2)))
  }
  canvas <- magick::image_blank(info$width, info$height, "white")
  img <- magick::image_composite(canvas, img,
                                 offset = sprintf("%+d%+d", as.integer(dx_mm * px), as.integer(dy_mm * px)))
  if (blur > 0) img <- magick::image_blur(img, 0, blur)
  magick::image_write(magick::image_convert(img, colorspace = "gray"), img_path,
                      format = "jpeg", quality = 85)
  img_path
}

skip_without_scan_tools <- function() {
  testthat::skip_if(Sys.which("pdftoppm") == "", "pdftoppm (poppler) not installed")
  testthat::skip_if(Sys.which("zbarimg") == "", "zbarimg (zbar) not installed")
}

# The example forms shipped with the package: four versions, six questions,
# four on the front and two on the back.
example_dir <- function() {
  d <- testthat::test_path("..", "..", "example", "six-question-quiz")
  if (!dir.exists(d)) testthat::skip("example forms not available (built package)")
  d
}

# Build the page JPEGs for one completed sheet from the blank forms.
#   zid:     digits as a string; "?" leaves that column empty.
#   answers: one string per question -- a letter, "" for blank, or two letters
#            for a double mark.
#   shade:   fill grey level (0 black .. 255 white); a vector recycles per mark.
#   distort: list(dx_mm, dy_mm, deg) passed to scannerise().
make_sheet <- function(forms_dir, cfg, version, zid, answers, work, id,
                       shade = 40, distort = list(), upside_down = FALSE) {
  form <- file.path(forms_dir, sprintf("quizform_v%s.pdf", version))
  lay <- suppressMessages(layout_from_form(cfg, form))
  grid <- load_id_grid_from_form(form, cfg)
  n_pages <- pdftools::pdf_length(form)
  out <- character(0)
  for (pg in seq_len(n_pages)) {
    jpg <- file.path(work, sprintf("sheet%02d-p%d.jpg", id, pg))
    form_page_jpeg(form, pg, jpg)
    uv_ans <- NULL
    for (q in cfg$questions) {
      if (lay$QUESTION_PAGE[[as.character(q)]] != pg) next
      for (letter in strsplit(answers[[q]], "")[[1]]) {
        uv_ans <- rbind(uv_ans, c(lay$ANSWER_X$col1[[letter]], lay$QUESTION_Y[[as.character(q)]]))
      }
    }
    if (!is.null(uv_ans)) fill_bubbles(jpg, uv_ans, radius = 0.011, shade = shade)
    if (pg == 1) {
      d <- strsplit(zid, "")[[1]]
      uv_id <- NULL
      for (ci in seq_along(d)) {
        if (d[ci] == "?") next
        uv_id <- rbind(uv_id, c(grid$x[[ci]], grid$y[[d[ci]]]))
      }
      if (!is.null(uv_id)) fill_bubbles(jpg, uv_id, radius = 0.0075, shade = 40)
    }
    do.call(scannerise, c(list(jpg), distort))
    if (upside_down) {
      magick::image_write(magick::image_rotate(magick::image_read(jpg), 180), jpg,
                          format = "jpeg", quality = 85)
    }
    out <- c(out, jpg)
  }
  out
}

# Run the whole marking pipeline on a scan PDF, the way a course folder does,
# and return the per-student sheets and the scored results.
run_pipeline <- function(pdf, config, forms_dir, key) {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  dir <- file.path(dirname(pdf), tools::file_path_sans_ext(basename(pdf)))
  suppressMessages(preprocess_scans(pdf, cfg, force = TRUE))
  suppressMessages(check_scan_sequence(dir))
  utils::capture.output(suppressMessages(
    mark_scans_cv(dir, config = cfg, layout = file.path(forms_dir, "layout.R"),
                  forms = forms_dir)))
  sheets <- suppressMessages(aggregate_sheets(dir, config = cfg))
  utils::capture.output(results <- suppressMessages(
    score_results(dir, key = key, config = cfg)))
  list(sheets = sheets, results = results, dir = dir)
}

answers_string <- function(sheets, cfg) {
  qs <- sheets[, paste0("q", cfg$questions), drop = FALSE]
  apply(qs, 1, function(r) paste(ifelse(is.na(r), "_", r), collapse = ""))
}
