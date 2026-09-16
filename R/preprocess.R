# preprocess.R -- turn a scanned PDF into page images plus the two CSVs that
# drive marking: progress.csv (one row per page, filled in by the marker) and
# overrides.csv (empty; where a human records manual corrections).

#' Prepare a scanned bubble sheet PDF for marking
#'
#' @param pdf Path to the scanned PDF (one page per student).
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param dpi Resolution for the extracted page images.
#' @param force Re-extract even if the output folder already exists.
#' @return Invisibly, the output directory.
#' @export
preprocess_scans <- function(pdf,
                             config = default_config_path(),
                             dpi    = 150,
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
  img_paths <- pdftools::pdf_convert(
    pdf_path, format = "jpeg", dpi = dpi,
    filenames = file.path(pages_dir, sprintf("page_%04d.jpeg", seq_len(n_pages)))
  )

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
    confidence   = NA_character_,
    notes        = NA_character_,
    error        = NA_character_
  )
  progress <- dplyr::bind_cols(progress, tibble::as_tibble(q_cols))

  csv_path <- file.path(out_dir, "progress.csv")
  readr::write_csv(progress, csv_path)

  overrides_path <- file.path(out_dir, "overrides.csv")
  readr::write_csv(tibble::tibble(
    page     = character(),
    zid      = character(),
    name     = character(),
    question = integer(),
    response = character()
  ), overrides_path)

  message(sprintf("Extracted %d page(s) to: %s", n_pages, out_dir))
  message("Progress CSV initialised:  ", csv_path)
  message("Overrides CSV initialised: ", overrides_path)
  message("\nNext step:")
  message("  bubblequiz mark --dir ", shQuote(out_dir))
  invisible(out_dir)
}
