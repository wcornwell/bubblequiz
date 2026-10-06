# scan_check.R -- validate the page sequence of a large scanned stack before
# any marking happens.
#
# A multi-page form is printed duplex, so a 150-student quiz arrives as one
# ~300-page PDF whose pages must read 1,2,1,2,... Sheet feeders drop pages,
# double-feed, and occasionally reverse a sheet. None of that is visible in the
# page images alone, so every page carries a QR giving its version, its page
# number and the sheet length. This file reads those and reports exactly where
# the stack stops making sense.

# Decode every QR on one page image. Returns a character vector of payloads,
# with a `corner` attribute naming the corner crop that found them (NA when the
# whole page decoded directly).
decode_qr_all <- function(img_path) {
  if (Sys.which("zbarimg") == "") {
    stop("zbarimg was not found on PATH. Install zbar to validate a scan stack.",
         call. = FALSE)
  }
  run <- function(path) {
    out <- suppressWarnings(
      system2("zbarimg", c("--quiet", "--raw", shQuote(path)),
              stdout = TRUE, stderr = FALSE)
    )
    out <- trimws(out[nzchar(trimws(out))])
    out[grepl("^bubblequiz", out)]
  }

  found <- run(img_path)
  if (length(found) > 0) return(structure(found, corner = NA_character_))

  # The page QR is small against the whole page, and at scan resolution its
  # modules are only two or three pixels wide; zbar misses it among everything
  # else on the page. Cropping to one corner and enlarging it decodes reliably.
  # The corner it turns up in is recorded, because a QR in the top-left means
  # the sheet went through the feeder upside down.
  img <- tryCatch(magick::image_read(img_path), error = function(e) NULL)
  if (is.null(img)) return(character(0))
  for (corner in names(QR_CORNERS)) {
    found <- decode_corner(img, corner, run)
    if (length(found) > 0) return(structure(found, corner = corner))
  }

  # Last resort: the whole page enlarged and sharpened.
  big <- magick::image_resize(img, geometry = "200%")
  big <- magick::image_convert(magick::image_contrast(big), colorspace = "gray")
  found <- run_image(big, run)
  if (length(found) > 0) return(structure(found, corner = NA_character_))
  character(0)
}

# Page-fraction boxes (x1, y1, x2, y2) searched for the page QR, most likely
# first. The QR is printed bottom-right; top-left is where it lands on a sheet
# scanned upside down.
QR_CORNERS <- list(
  br = c(0.70, 0.78, 1.00, 1.00),
  tl = c(0.00, 0.00, 0.30, 0.22),
  bl = c(0.00, 0.78, 0.30, 1.00),
  tr = c(0.70, 0.00, 1.00, 0.22)
)

qr_corner_crop <- function(img, corner) {
  box <- QR_CORNERS[[corner]]
  info <- magick::image_info(img)
  x1 <- as.integer(box[1] * info$width);  y1 <- as.integer(box[2] * info$height)
  x2 <- as.integer(box[3] * info$width);  y2 <- as.integer(box[4] * info$height)
  crop <- magick::image_crop(img, sprintf("%dx%d+%d+%d", x2 - x1, y2 - y1, x1, y1))
  magick::image_convert(crop, colorspace = "gray")
}

# Ways of presenting a corner crop to zbar, tried in order. At 150-200 dpi a QR
# module is two or three pixels wide and blurred by JPEG; a smooth enlargement
# followed by a hard threshold rebuilds clean module edges. Nearest-neighbour
# enlargement does not: it keeps the blur and decodes almost nothing.
QR_ENHANCERS <- list(
  as_is = function(img) img,
  x4_threshold = function(img) binarise(magick::image_resize(img, "400%"), "50%"),
  x3_threshold = function(img) binarise(magick::image_resize(img, "300%"), "55%"),
  # Darker cut-offs recover a QR whose printing is faded or smudged; lighter
  # ones a QR printed heavy. QR error correction means a variant either reads
  # the true payload or reads nothing.
  x4_dark = function(img) binarise(magick::image_resize(img, "400%"), "40%"),
  x3_dark = function(img) binarise(magick::image_resize(img, "300%"), "45%"),
  x2_light = function(img) binarise(magick::image_resize(img, "200%"), "60%")
)

binarise <- function(img, level) {
  img <- magick::image_threshold(img, type = "white", threshold = level)
  magick::image_threshold(img, type = "black", threshold = level)
}

# Decode a corner crop, trying each enhancement in turn.
decode_corner <- function(img, corner, run) {
  crop <- qr_corner_crop(img, corner)
  for (enh in QR_ENHANCERS) {
    found <- run_image(enh(crop), run)
    if (length(found) > 0) return(found)
  }
  character(0)
}

run_image <- function(img, run) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({ magick::image_write(img, tmp, format = "png"); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(character(0))
  run(tmp)
}

# Reduce the payloads found on one page to a single reading. Pages carry more
# than one QR by design; they must agree.
page_reading <- function(payloads) {
  if (length(payloads) == 0) {
    return(list(version = NA_character_, page = NA_integer_,
                pages = NA_integer_, note = "no QR decoded"))
  }
  fields <- lapply(payloads, parse_qr_payload)
  versions <- unique(vapply(fields, function(f) f$version %||% NA_character_, character(1)))
  versions <- versions[!is.na(versions)]
  # Only the footer QR carries page/pages; the header QR legitimately omits it.
  pages_no <- unique(stats::na.omit(vapply(fields, function(f) {
    suppressWarnings(as.integer(f$page %||% NA))
  }, integer(1))))
  pages_of <- unique(stats::na.omit(vapply(fields, function(f) {
    suppressWarnings(as.integer(f$pages %||% NA))
  }, integer(1))))

  note <- character(0)
  if (length(versions) > 1) note <- c(note, "conflicting versions on one page")
  if (length(pages_no) > 1) note <- c(note, "conflicting page numbers on one page")

  list(
    version = if (length(versions) == 1) versions else NA_character_,
    page    = if (length(pages_no) == 1) pages_no else NA_integer_,
    pages   = if (length(pages_of) == 1) pages_of else NA_integer_,
    note    = paste(note, collapse = "; ")
  )
}

# Walk a decoded stack and group it into sheets. Split out from
# check_scan_sequence() so the grouping logic can be tested without images.
# A sheet is accepted only if its pages run 1..pages with a single version.
# Anything else is flagged at the page where it went wrong and the walk resumes
# at the next page reading page 1, so one feeder error does not cascade through
# the rest of the stack.
#
# For a two-page form one more fact is available: a duplex scanner emits each
# physical sheet as stack positions (1,2), (3,4), ... Two recoveries rest on
# that, and only apply at those positions:
#   - a sheet fed back side first reads 2,1; it is paired, in page order.
#   - a side whose QR cannot be read takes its version and page number from
#     the other side of the same paper.
# Both are reported in `status` (prefixed "ok:") so the sheet can be checked
# by a person; neither ever joins pages across two physical sheets.
sequence_walk <- function(page_no, pages, version, note = NULL) {
  n <- length(page_no)
  if (is.null(note)) note <- rep("", n)
  sheet  <- rep(NA_integer_, n)
  status <- rep(NA_character_, n)
  recovered <- rep("", n)

  duplex_pairs <- length(stats::na.omit(pages)) > 0 && all(stats::na.omit(pages) == 2L)
  if (duplex_pairs && n >= 2L) {
    for (a in seq(1L, n - 1L, by = 2L)) {
      b <- a + 1L
      if (is.na(page_no[a]) != is.na(page_no[b])) {
        known <- if (is.na(page_no[a])) b else a
        lost  <- if (known == a) b else a
        if (!is.na(version[known]) && page_no[known] %in% 1:2) {
          page_no[lost] <- 3L - page_no[known]
          version[lost] <- version[known]
          pages[lost]   <- 2L
          recovered[lost] <- "QR unreadable; identified from the other side of the sheet"
        }
      }
    }
  }

  i <- 1L
  sheet_id <- 0L
  while (i <= n) {
    if (is.na(page_no[i])) {
      status[i] <- if (nzchar(note[i])) note[i] else "unreadable QR"
      i <- i + 1L
      next
    }
    if (duplex_pairs && i %% 2L == 1L && i < n && identical(page_no[i], 2L) &&
        identical(page_no[i + 1L], 1L) && !is.na(version[i]) &&
        identical(version[i], version[i + 1L])) {
      sheet_id <- sheet_id + 1L
      sheet[i:(i + 1L)] <- sheet_id
      status[i:(i + 1L)] <- "ok: sheet scanned back side first"
      i <- i + 2L
      next
    }
    if (page_no[i] != 1L) {
      status[i] <- sprintf("orphan page %d (no page 1 before it)", page_no[i])
      i <- i + 1L
      next
    }
    len <- pages[i]
    if (is.na(len) || len < 1L) {
      status[i] <- "sheet length missing from QR"
      i <- i + 1L
      next
    }
    idx <- i:min(n, i + len - 1L)
    expected <- seq_len(len)
    got <- page_no[idx]
    vs <- unique(stats::na.omit(version[idx]))
    ok <- length(idx) == len &&
      identical(as.integer(got), as.integer(expected)) &&
      length(vs) == 1L
    sheet_id <- sheet_id + 1L
    if (ok) {
      sheet[idx] <- sheet_id
      status[idx] <- ifelse(nzchar(recovered[idx]), paste0("ok: ", recovered[idx]), "ok")
      i <- i + len
    } else {
      why <- if (length(idx) != len) {
        "stack ends mid-sheet"
      } else if (length(vs) > 1L) {
        paste0("version changes within sheet (", paste(vs, collapse = "/"), ")")
      } else {
        sprintf("expected pages %s, read %s",
                paste(expected, collapse = ","),
                paste(ifelse(is.na(got), "?", got), collapse = ","))
      }
      status[idx] <- paste("BROKEN:", why)
      i <- i + 1L
    }
  }
  list(sheet = sheet, status = status, page_no = page_no, version = version)
}

#' Validate the page sequence of a scanned stack
#'
#' Reads the QR on every page of a scan and checks that the stack is made of
#' whole sheets: each sheet starts at page 1, runs to the sheet length printed
#' in the QR, and keeps one version throughout. Run this before
#' [mark_scans_cv()] so a feeder error is caught while the paper is still on the
#' desk, rather than as silently misattributed answers.
#'
#' @param scans Path to the scan folder produced by [preprocess_scans()], or a
#'   folder of page images.
#' @param output CSV path for the per-page report.
#' @return Invisibly, a data frame with one row per page. Columns: `page_index`
#'   (position in the stack), `file`, `version`, `page_no`, `pages`, `sheet`
#'   (assigned sheet number, NA where the stack is broken) and `status`.
#' @export
check_scan_sequence <- function(scans, output = file.path(scans, "scan_sequence.csv")) {
  pages_dir <- if (dir.exists(file.path(scans, "pages"))) file.path(scans, "pages") else scans
  imgs <- sort(list.files(pages_dir, pattern = "[.](png|jpe?g|tiff?)$",
                          full.names = TRUE, ignore.case = TRUE))
  if (length(imgs) == 0) stop("No page images found in: ", pages_dir, call. = FALSE)

  message("Reading QR codes from ", length(imgs), " page(s)...")
  readings <- lapply(seq_along(imgs), function(i) {
    if (i %% 50 == 0) message("  ", i, "/", length(imgs))
    on.exit(free_page_images())
    page_reading(decode_qr_all(imgs[i]))
  })

  version <- vapply(readings, `[[`, character(1), "version")
  page_no <- vapply(readings, `[[`, integer(1), "page")
  pages   <- vapply(readings, `[[`, integer(1), "pages")
  note    <- vapply(readings, `[[`, character(1), "note")

  n <- length(imgs)
  walk <- sequence_walk(page_no, pages, version, note)
  sheet   <- walk$sheet
  status  <- walk$status
  page_no <- walk$page_no
  version <- walk$version

  df <- data.frame(
    page_index = seq_len(n),
    file       = basename(imgs),
    version    = version,
    page_no    = page_no,
    pages      = pages,
    sheet      = sheet,
    status     = status,
    stringsAsFactors = FALSE
  )

  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(df, output, row.names = FALSE)

  bad <- df[!startsWith(df$status, "ok"), , drop = FALSE]
  noted <- df[startsWith(df$status, "ok:"), , drop = FALSE]
  n_sheets <- length(unique(stats::na.omit(df$sheet)))
  message("\nPages:  ", n)
  message("Sheets: ", n_sheets, " complete")
  if (nrow(noted) > 0) {
    message("Recovered (sheets flagged for review): ", nrow(noted), " page(s)")
    for (r in seq_len(nrow(noted))) {
      message(sprintf("  page %-4d %-22s %s",
                      noted$page_index[r], noted$file[r], sub("^ok: ", "", noted$status[r])))
    }
  }
  if (nrow(bad) == 0) {
    message("Sequence is clean; every page belongs to a complete sheet.")
  } else {
    message("PROBLEM PAGES: ", nrow(bad), " -- rescan or handle these before marking")
    for (r in seq_len(min(nrow(bad), 20L))) {
      message(sprintf("  page %-4d %-22s %s",
                      bad$page_index[r], bad$file[r], bad$status[r]))
    }
    if (nrow(bad) > 20) message("  ... ", nrow(bad) - 20, " more; see ", output)
  }
  message("Report written: ", output)
  invisible(df)
}
