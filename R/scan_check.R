# scan_check.R -- validate the page sequence of a large scanned stack before
# any marking happens.
#
# A multi-page form is printed duplex, so a 150-student quiz arrives as one
# ~300-page PDF whose pages must read 1,2,1,2,... Sheet feeders drop pages,
# double-feed, and occasionally reverse a sheet. None of that is visible in the
# page images alone, so every page carries a QR giving its version, its page
# number and the sheet length. This file reads those and reports exactly where
# the stack stops making sense.

# Decode every QR on one page image. Returns a character vector of payloads.
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
  if (length(found) > 0) return(found)

  # A QR that is small relative to the scan resolution can fall below the
  # decoder's threshold. Retry once on an upscaled, sharpened copy before
  # reporting the page as unreadable.
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({
    img <- magick::image_read(img_path)
    img <- magick::image_resize(img, geometry = "200%")
    img <- magick::image_convert(magick::image_contrast(img), colorspace = "gray")
    magick::image_write(img, tmp, format = "png")
    TRUE
  }, error = function(e) FALSE)
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
sequence_walk <- function(page_no, pages, version, note = NULL) {
  n <- length(page_no)
  if (is.null(note)) note <- rep("", n)
  sheet  <- rep(NA_integer_, n)
  status <- rep(NA_character_, n)

  i <- 1L
  sheet_id <- 0L
  while (i <= n) {
    if (is.na(page_no[i])) {
      status[i] <- if (nzchar(note[i])) note[i] else "unreadable QR"
      i <- i + 1L
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
      status[idx] <- "ok"
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
  list(sheet = sheet, status = status)
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
    page_reading(decode_qr_all(imgs[i]))
  })

  version <- vapply(readings, `[[`, character(1), "version")
  page_no <- vapply(readings, `[[`, integer(1), "page")
  pages   <- vapply(readings, `[[`, integer(1), "pages")
  note    <- vapply(readings, `[[`, character(1), "note")

  n <- length(imgs)
  walk <- sequence_walk(page_no, pages, version, note)
  sheet  <- walk$sheet
  status <- walk$status

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

  bad <- df[df$status != "ok", , drop = FALSE]
  n_sheets <- length(unique(stats::na.omit(df$sheet)))
  message("\nPages:  ", n)
  message("Sheets: ", n_sheets, " complete")
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
