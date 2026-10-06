# Turning a scanned PDF into page images. Scanners write each page as a JPEG
# image; the poppler inside pdftools has rendered those as blank white pages
# without any error, which made every page read as unanswered.

test_that("scanner-style PDFs render with ink on every page", {
  skip_without_scan_tools()
  work <- withr::local_tempdir()
  form <- file.path(example_dir(), "output", "quizform_v1.pdf")
  jpgs <- c(form_page_jpeg(form, 1, file.path(work, "a.jpg")),
            form_page_jpeg(form, 2, file.path(work, "b.jpg")))
  for (flate in c(TRUE, FALSE)) {
    pdf <- write_scan_pdf(jpgs, file.path(work, sprintf("scan-%s.pdf", flate)), flate = flate)
    out <- render_pdf_pages(pdf, dpi = 100, format = "png",
                            filenames = file.path(work, sprintf("p%s-%d.png", flate, 1:2)))
    expect_true(all(file.exists(out)))
    expect_false(any(vapply(out, page_is_blank, logical(1))))
  }
})

test_that("pages come out in page order", {
  skip_without_scan_tools()
  work <- withr::local_tempdir()
  form <- file.path(example_dir(), "output", "quizform_v1.pdf")
  # Twelve pages, so pdftoppm pads page numbers to two digits.
  jpgs <- vapply(1:12, function(i) {
    form_page_jpeg(form, if (i %% 2) 1 else 2, file.path(work, sprintf("s%02d.jpg", i)))
  }, character(1))
  pdf <- write_scan_pdf(jpgs, file.path(work, "scan.pdf"))
  out <- render_pdf_pages(pdf, dpi = 60, format = "png",
                          filenames = file.path(work, sprintf("page_%04d.png", 1:12)))
  # Fronts carry the zID grid, backs do not: the fronts are the darker pages.
  ink <- vapply(out, function(p) {
    mean(255 - as.integer(magick::image_data(magick::image_convert(
      magick::image_read(p), colorspace = "gray"), "gray")))
  }, numeric(1))
  fronts <- ink[c(TRUE, FALSE)]; backs <- ink[c(FALSE, TRUE)]
  expect_true(min(fronts) > max(backs))
})

test_that("a blank render stops preprocessing with a clear error", {
  work <- withr::local_tempdir()
  blank <- file.path(work, "page_0001.png")
  magick::image_write(magick::image_blank(400, 566, "white"), blank)
  expect_true(page_is_blank(blank))
  expect_error(check_pages_not_blank(blank), "blank")
})
