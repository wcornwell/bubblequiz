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

test_that("a page with markers cut off is registered correctly or reported, never misplaced", {
  # Shifted 5.5 mm right, the right-hand squares (printed 3-8 mm from the edge)
  # are cut off and must not be used. What is left either places the page
  # correctly, or the page is reported as out of register.
  cfg <- load_exam_config(file.path(example_dir(), "exam.yml"))
  L <- suppressMessages(form_layouts(cfg, file.path(example_dir(), "output")))[["1"]]
  work <- withr::local_tempdir()
  ref <- build_map_xy(page_png(attr(L, "form_pdf"), 1, work, "ref.png"))
  f <- file.path(work, "clipped.jpg")
  magick::image_write(magick::image_read(page_png(attr(L, "form_pdf"), 1, work, "src.png")),
                      f, format = "jpeg")
  scannerise(f, dx_mm = 5.5)
  g <- gray_matrix(magick::image_read(f))
  pts <- detect_corner_markers(g, ncol(g), nrow(g))
  expect_true(all(is.na(pts[c("tr", "br"), ])))       # the cut-off ones
  reg <- register_page(f, cfg, L, 1L)
  if (reg$registered) {
    px <- reg$ctx$w / 210
    d <- reg$ctx$map_xy(0.5, 0.5) - ref$map_xy(0.5, 0.5)
    expect_lt(abs(unname(d["x"]) - 5.5 * px), 3)
    expect_lt(abs(unname(d["y"])), 3)
  } else {
    expect_match(reg$note, "rescan it straight")
  }
})

test_that("a banner or text block is never taken for a missing marker", {
  # With the top-right marker cut off, the black Version banner on the example
  # form sits in its search window. It must not be used.
  work <- withr::local_tempdir()
  form <- file.path(example_dir(), "output", "quizform_v1.pdf")
  f <- file.path(work, "c.jpg")
  magick::image_write(magick::image_read(page_png(form, 1, work, "src.png")), f, format = "jpeg")
  scannerise(f, dx_mm = 5.5)
  g <- gray_matrix(magick::image_read(f))
  pts <- detect_corner_markers(g, ncol(g), nrow(g))
  expect_true(is.na(pts["tr", "x"]))
})

test_that("a sheet fed crooked is registered through its rotation", {
  cfg <- load_exam_config(file.path(example_dir(), "exam.yml"))
  L <- suppressMessages(form_layouts(cfg, file.path(example_dir(), "output")))[["1"]]
  work <- withr::local_tempdir()
  f <- file.path(work, "skew.jpg")
  magick::image_write(magick::image_read(page_png(attr(L, "form_pdf"), 1, work, "src.png")),
                      f, format = "jpeg")
  scannerise(f, deg = 1.5)
  ctx <- build_map_xy(f)
  expect_true(ctx$marker_ok)
  expect_equal(page_angle(ctx$map_xy), 1.5, tolerance = 0.2)
  blank <- blank_form_ctx(attr(L, "form_pdf"), 1L, ctx$w)
  off <- registration_offsets(ctx, blank, registration_anchors(cfg, L, 1L))
  expect_false(anyNA(off))
  expect_true(all(abs(off) <= 2))
})

test_that("the registration check measures how far the page is out", {
  cfg <- load_exam_config(file.path(example_dir(), "exam.yml"))
  forms <- file.path(example_dir(), "output")
  L <- suppressMessages(form_layouts(cfg, forms))[["1"]]
  work <- withr::local_tempdir()
  f <- page_png(attr(L, "form_pdf"), 1, work, "p.png")
  ctx <- build_map_xy(f)
  blank <- blank_form_ctx(attr(L, "form_pdf"), 1L, ctx$w)
  anchors <- registration_anchors(cfg, L, 1L)
  expect_gt(nrow(anchors), 1)
  expect_true(all(abs(registration_offsets(ctx, blank, anchors)) <= 1))
  # A mapping that is wrong by 6 px right and 3 px down is measured as such.
  bad <- ctx
  bad$map_xy <- function(u, v) {
    m <- ctx$map_xy(u, v)
    if (is.matrix(m)) sweep(m, 2, c(6, 3)) else m - c(x = 6, y = 3)
  }
  off <- registration_offsets(bad, blank, anchors)
  expect_true(all(abs(off[, "dx"] - 6) <= 1))
  expect_true(all(abs(off[, "dy"] - 3) <= 1))
})
