# Reading the page QR codes, which say what version and page each scan is.

scan_page <- function(work, page = 1, ...) {
  form <- file.path(example_dir(), "output", "quizform_v2.pdf")
  jpg <- form_page_jpeg(form, page, file.path(work, sprintf("p%d.jpg", page)))
  scannerise(jpg, ...)
  png <- file.path(work, sprintf("p%d.png", page))
  magick::image_write(magick::image_read(jpg), png, format = "png")
  png
}

test_that("the page QR decodes on a 200 dpi scan", {
  skip_without_scan_tools()
  work <- withr::local_tempdir()
  for (pg in 1:2) {
    found <- decode_qr_all(scan_page(work, pg, dx_mm = 1, dy_mm = 1))
    expect_length(found, 1)
    fields <- parse_qr_payload(found[[1]])
    expect_equal(fields$version, "2")
    expect_equal(fields$page, as.character(pg))
  }
})

test_that("an upside-down page is detected and turned round", {
  skip_without_scan_tools()
  work <- withr::local_tempdir()
  png <- scan_page(work, 1)
  expect_equal(page_qr_corner(png), "br")
  magick::image_write(magick::image_rotate(magick::image_read(png), 180), png)
  expect_equal(page_qr_corner(png), "tl")
  expect_equal(fix_upside_down_pages(png), png)
  expect_equal(page_qr_corner(png), "br")
})

test_that("a page with no QR decodes to nothing", {
  skip_without_scan_tools()
  work <- withr::local_tempdir()
  f <- file.path(work, "blank.png")
  magick::image_write(magick::image_blank(1654, 2339, "white"), f)
  expect_length(decode_qr_all(f), 0)
})

test_that("a QR with faded printing still decodes", {
  # Bottom-right corner of a real scanned page whose QR printed with a faded,
  # smudged patch. The first enhancements miss it; a darker threshold reads it.
  skip_without_scan_tools()
  img <- magick::image_read(test_path("fixtures", "qr-faded-corner.png"))
  run <- function(path) {
    out <- suppressWarnings(system2("zbarimg", c("--quiet", "--raw", shQuote(path)),
                                    stdout = TRUE, stderr = FALSE))
    out[grepl("^bubblequiz", trimws(out))]
  }
  found <- character(0)
  for (enh in QR_ENHANCERS) {
    found <- run_image(enh(img), run)
    if (length(found)) break
  }
  expect_length(found, 1)
  fields <- parse_qr_payload(found[[1]])
  expect_equal(fields$version, "4")
  expect_equal(fields$page, "1")
})
