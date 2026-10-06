# preprocess.R -- turn a scanned PDF into page images plus the two CSVs that
# drive marking: progress.csv (one row per page, filled in by the marker) and
# overrides.csv (empty; where a human records manual corrections).

#' Prepare a scanned bubble sheet PDF for marking
#'
#' @param pdf Path to the scanned PDF (one page per student).
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param dpi Resolution for the extracted page images. 200 matches the usual
#'   scanner setting; much lower and the page QR codes stop decoding reliably.
#' @param format Image format for extracted pages, usually `"png"` or `"jpeg"`.
#' @param force Re-extract even if the output folder already exists.
#' @return Invisibly, the output directory.
#' @export
preprocess_scans <- function(pdf,
                             config = default_config_path(),
                             dpi    = 200,
                             format = "png",
                             force  = FALSE) {
  cfg <- if (is.list(config)) config else load_exam_config(config)

  if (!file.exists(pdf)) stop("PDF not found: ", pdf, call. = FALSE)
  pdf_path <- normalizePath(pdf)
  out_dir  <- file.path(dirname(pdf_path), tools::file_path_sans_ext(basename(pdf_path)))

  if (dir.exists(out_dir) && !force) {
    stop("Output folder already exists: ", out_dir,
         "\nDelete it or pass force = TRUE to re-extract.", call. = FALSE)
  }
  pages_dir <- file.path(out_dir, "pages")
  dir.create(pages_dir, showWarnings = FALSE, recursive = TRUE)

  message("Converting PDF to images (DPI=", dpi, "): ", pdf_path)
  n_pages   <- pdftools::pdf_length(pdf_path)
  format <- tolower(format)
  ext <- if (format %in% c("jpg", "jpeg")) "jpeg" else format
  img_paths <- render_pdf_pages(
    pdf_path, dpi = dpi, format = format,
    filenames = file.path(pages_dir, sprintf("page_%04d.%s", seq_len(n_pages), ext))
  )
  check_pages_not_blank(img_paths)
  rotated <- fix_upside_down_pages(img_paths)
  if (length(rotated)) {
    message("Rotated ", length(rotated), " upside-down page(s): ",
            paste(basename(rotated), collapse = ", "))
  }

  q_cols <- stats::setNames(
    rep(list(NA_character_), length(cfg$questions)),
    paste0("q", cfg$questions)
  )

  progress <- tibble::tibble(
    status       = "pending",
    api_call_ok  = NA,
    file         = file.path("pages", basename(img_paths)),
    page         = seq_len(n_pages),
    zid          = NA_character_,
    name         = NA_character_,
    needs_review = NA,
    exam_version = NA_character_,
    # Filled in by check_scan_sequence(); a single-page form leaves them NA.
    sheet        = NA_integer_,
    sheet_page   = NA_integer_,
    confidence   = NA_character_,
    notes        = NA_character_,
    error        = NA_character_
  )
  progress <- dplyr::bind_cols(progress, tibble::as_tibble(q_cols))

  csv_path <- file.path(out_dir, "progress.csv")
  readr::write_csv(progress, csv_path)

  # Never overwrite overrides.csv: it holds a person's review of the sheets, and
  # re-extracting the pages must not throw that work away.
  overrides_path <- file.path(out_dir, "overrides.csv")
  if (!file.exists(overrides_path)) {
    readr::write_csv(tibble::tibble(
      file     = character(),
      page     = character(),
      zid      = character(),
      name     = character(),
      question = character(),
      response = character()
    ), overrides_path)
  } else {
    message("Keeping existing overrides: ", overrides_path)
  }

  message(sprintf("Extracted %d page(s) to: %s", n_pages, out_dir))
  message("Progress CSV initialised:  ", csv_path)
  message("Overrides CSV initialised: ", overrides_path)
  message("\nNext step:")
  message("  bubblequiz mark --dir ", shQuote(out_dir))
  invisible(out_dir)
}

# render_pdf_pages: rasterise every page of a scanned PDF.
#
# Poppler's command-line pdftoppm is preferred. The poppler bundled inside the
# pdftools R package can silently render JPEG page images -- which is what every
# scanner produces -- as blank white pages, with no error; pdftools is only the
# fallback when pdftoppm is not installed.
render_pdf_pages <- function(pdf, dpi, format, filenames) {
  pdftoppm <- Sys.which("pdftoppm")
  if (nzchar(pdftoppm)) {
    fmt_flag <- if (format %in% c("jpg", "jpeg")) "-jpeg" else "-png"
    prefix <- tempfile("page-")
    status <- system2(pdftoppm, c("-r", dpi, fmt_flag, "-gray", shQuote(pdf), shQuote(prefix)),
                      stdout = FALSE, stderr = FALSE)
    rendered <- sort(Sys.glob(paste0(prefix, "-*")))
    if (identical(as.integer(status), 0L) && length(rendered) == length(filenames)) {
      # pdftoppm pads page numbers to the page count's width; the sort above
      # therefore gives page order.
      ok <- file.rename(rendered, filenames)
      if (!all(ok)) ok <- file.copy(rendered, filenames, overwrite = TRUE)
      unlink(rendered)
      if (all(ok)) return(filenames)
    }
    unlink(rendered)
    warning("pdftoppm failed on ", pdf, "; falling back to pdftools.", call. = FALSE)
  }
  pdftools::pdf_convert(pdf, format = format, dpi = dpi,
                        filenames = filenames, verbose = FALSE)
}

# check_pages_not_blank: refuse to continue from a render that produced empty
# pages. Every printed form carries black corner markers and a QR, so a page
# with no dark pixels at all was not rendered, whatever the student wrote.
check_pages_not_blank <- function(img_paths) {
  blank <- vapply(img_paths, function(p) {
    on.exit(free_page_images())
    page_is_blank(p)
  }, logical(1))
  if (any(blank)) {
    stop(sum(blank), " of ", length(img_paths), " rendered page(s) are blank (",
         paste(utils::head(basename(img_paths[blank]), 5), collapse = ", "),
         if (sum(blank) > 5) ", ..." else "", ").\n",
         "The PDF renderer could not draw the scanned images. Install poppler ",
         "(which provides pdftoppm) and run again.", call. = FALSE)
  }
  invisible(TRUE)
}

page_is_blank <- function(path) {
  img <- magick::image_read(path)
  img <- magick::image_convert(magick::image_scale(img, "400"), colorspace = "gray")
  vals <- as.integer(magick::image_data(img, channels = "gray"))
  # Fewer than 0.1% of pixels darker than mid-grey.
  mean(vals < 128) < 0.001
}

# fix_upside_down_pages: turn round any page fed through the scanner upside
# down, so everything after this step can assume the printed orientation. The
# page QR is printed bottom-right; found top-left, the page is inverted.
fix_upside_down_pages <- function(img_paths) {
  if (Sys.which("zbarimg") == "") return(character(0))
  rotated <- character(0)
  for (p in img_paths) {
    if (identical(page_qr_corner(p), "tl")) {
      img <- magick::image_rotate(magick::image_read(p), 180)
      magick::image_write(img, p)
      rotated <- c(rotated, p)
    }
    free_page_images()
  }
  rotated
}

# page_qr_corner: which corner of the page its QR was found in, or NA.
page_qr_corner <- function(img_path) {
  run <- function(path) {
    out <- suppressWarnings(system2("zbarimg", c("--quiet", "--raw", shQuote(path)),
                                    stdout = TRUE, stderr = FALSE))
    out[grepl("^bubblequiz", trimws(out))]
  }
  img <- magick::image_read(img_path)
  for (corner in c("br", "tl")) {
    if (length(decode_corner(img, corner, run)) > 0) return(corner)
  }
  NA_character_
}
