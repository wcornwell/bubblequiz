# Locating the four corner squares. Everything the marker reads is positioned
# through them, so an error here moves every bubble.

page_png <- function(form, page, work, name, dpi = 200) {
  f <- file.path(work, name)
  magick::image_write(magick::image_flatten(magick::image_background(
    magick::image_read_pdf(form, pages = page, density = dpi), "white")), f)
  f
}

test_that("markers are found at their printed centres", {
  work <- withr::local_tempdir()
  form <- file.path(example_dir(), "output", "quizform_v1.pdf")
  for (pg in 1:2) {
    ctx <- build_map_xy(page_png(form, pg, work, sprintf("p%d.png", pg)))
    expect_true(ctx$marker_ok)
    # Printed at 5.5 mm from each edge.
    px <- ctx$w / 210
    tl <- ctx$map_xy(REG_MARKERS_REF["tl", "x"], REG_MARKERS_REF["tl", "y"])
    expect_equal(unname(tl["x"]), 5.5 * px, tolerance = 1.5 / (5.5 * px))
    expect_equal(unname(tl["y"]), 5.5 * px, tolerance = 1.5 / (5.5 * px))
  }
})

test_that("a shifted scan maps every point by the same shift", {
  # The top-left marker sits beside the black Version banner on the front
  # page. The detector once averaged the banner in, which skewed the fit by
  # a bubble's width across the zID grid; this is the page that exposed it.
  work <- withr::local_tempdir()
  form <- file.path(example_dir(), "output", "quizform_v1.pdf")
  ref <- build_map_xy(page_png(form, 1, work, "ref.png"))
  shifted <- file.path(work, "shifted.jpg")
  file.copy(page_png(form, 1, work, "src.png"), shifted)
  magick::image_write(magick::image_read(shifted), shifted, format = "jpeg")
  scannerise(shifted, dx_mm = 2.5, dy_mm = 1.5, blur = 0.6)
  ctx <- build_map_xy(shifted)
  expect_true(ctx$marker_ok)
  px <- ctx$w / 210
  for (uv in list(c(0.2, 0.1), c(0.8, 0.1), c(0.5, 0.5), c(0.9, 0.9))) {
    d <- ctx$map_xy(uv[1], uv[2]) - ref$map_xy(uv[1], uv[2])
    expect_equal(unname(d["x"]), as.integer(2.5 * px), tolerance = 1.5 / (2.5 * px))
    expect_equal(unname(d["y"]), as.integer(1.5 * px), tolerance = 1.5 / (1.5 * px))
  }
})

test_that("a page with no markers reports failure instead of guessing", {
  work <- withr::local_tempdir()
  f <- file.path(work, "blank.png")
  magick::image_write(magick::image_blank(1654, 2339, "white"), f)
  expect_false(build_map_xy(f)$marker_ok)
})

test_that("a marker cut off by the edge of the scan fails registration", {
  # Shifted 5 mm right, the right-hand squares (printed 3-8 mm from the edge)
  # are half off the page. Their centroids would be wrong by pixels, so the
  # page must be reported as unregistered rather than read slightly off.
  work <- withr::local_tempdir()
  form <- file.path(example_dir(), "output", "quizform_v1.pdf")
  f <- file.path(work, "clipped.jpg")
  magick::image_write(magick::image_read(page_png(form, 1, work, "src.png")), f, format = "jpeg")
  scannerise(f, dx_mm = 5.5)
  expect_false(build_map_xy(f)$marker_ok)
})
