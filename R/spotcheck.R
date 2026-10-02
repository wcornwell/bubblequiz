# Spot checks: a page to confirm by eye that registration and reading work.

#' Build a spot-check page for a marked quiz
#'
#' Marking trusts two things a person can check at a glance: that each page
#' was lined up with its form (registration), and that the bubbles read as
#' filled are the ones the student filled. This writes an HTML page, with one
#' overlay image per scanned page, for a sample of sheets:
#'
#' * every sheet with a page the registration check refused, to confirm each
#'   really was fed crooked (and to read its answers by hand);
#' * the sheets whose pages passed closest to the tolerance, where a page
#'   that should have been refused would show up;
#' * sheets whose zID is not on the class list, when `roster` is given;
#' * a random sample of sheets that went to the upload untouched.
#'
#' On each overlay, boxes mark the printed anchors ("Answer Qn", the zID
#' heading) where the page map puts them: green within tolerance, red not. A
#' page is registered correctly when every box sits on its printed text. Thin
#' grey rings are every bubble; thick blue rings are the ones read as filled
#' (orange: read as uncertain). The rings should sit on the bubbles, and the
#' blue ones on the pen marks.
#'
#' Registration is measured for every page once and kept in
#' `registration.csv` in each scan folder, so a second run is quick.
#'
#' @param folder Folder holding the scan PDFs and their scan folders.
#' @param config Path to the exam config YAML.
#' @param forms Folder holding `quizform_v<N>.pdf` for every version.
#' @param n Number of random clean sheets.
#' @param n_borderline Number of passed sheets closest to the tolerance.
#' @param roster Optional Moodle grades export (or any CSV with `Username`,
#'   `First name`, `Last name`), to show the enrolled name beside each zID.
#' @param review Optional review file, to show its suggested zIDs.
#' @param seed Seed for the random sample.
#' @param output Path for the HTML page; overlays go in a folder beside it.
#' @return Invisibly, the per-page registration table.
#' @export
spot_check <- function(folder,
                       config = "exam.yml",
                       forms = "output",
                       n = 8,
                       n_borderline = 6,
                       roster = NULL,
                       review = NULL,
                       seed = 1,
                       output = file.path(folder, "spot_check.html")) {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  layouts <- form_layouts(cfg, forms)
  pdfs <- sort(list.files(folder, pattern = "[.]pdf$", full.names = TRUE, ignore.case = TRUE))
  dirs <- file.path(dirname(pdfs), tools::file_path_sans_ext(basename(pdfs)))
  dirs <- dirs[file.exists(file.path(dirs, "sheets.csv"))]
  if (!length(dirs)) stop("No marked scan folders in ", folder, call. = FALSE)

  reg <- do.call(rbind, lapply(dirs, page_registration, cfg = cfg, layouts = layouts))
  tol <- REGISTRATION_TOLERANCE_PX

  sheets <- do.call(rbind, lapply(dirs, function(d) {
    s <- utils::read.csv(file.path(d, "sheets.csv"), stringsAsFactors = FALSE)
    r <- utils::read.csv(file.path(d, "results.csv"), stringsAsFactors = FALSE)
    m <- match(s$file, r$file)
    data.frame(scan = basename(d), files = s$files, zid = r$zid[m], score = r$score[m],
               answers = r$answers[m], needs_review = r$needs_review[m] %in% TRUE,
               version = as.character(r$exam_version[m]), stringsAsFactors = FALSE)
  }))
  names_of <- roster_names(roster)
  suggested <- review_suggestions(review)

  page_rows <- function(i) reg[reg$scan == sheets$scan[i] &
                                 reg$file %in% strsplit(sheets$files[i], ";")[[1]], , drop = FALSE]
  worst <- vapply(seq_len(nrow(sheets)), function(i) {
    e <- page_rows(i)$error; if (length(e)) max(e) else NA_real_
  }, numeric(1))
  refused <- which(vapply(seq_len(nrow(sheets)), function(i) any(!page_rows(i)$registered), logical(1)))
  passed <- setdiff(which(is.finite(worst)), refused)
  borderline <- utils::head(passed[order(-worst[passed])], n_borderline)
  off_roster <- if (length(names_of)) {
    which(!sheets$needs_review & grepl(zid_regex(cfg), sheets$zid) & !sheets$zid %in% names(names_of))
  } else integer(0)
  clean <- setdiff(which(!sheets$needs_review), c(borderline, off_roster))
  set.seed(seed)
  random <- clean[sample.int(length(clean), min(n, length(clean)))]

  img_dir <- sub("[.]html?$", "_files", output)
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  card <- function(i, why) {
    pages <- page_rows(i)
    imgs <- vapply(seq_len(nrow(pages)), function(k) {
      p <- pages[k, ]
      out <- file.path(img_dir, sprintf("%s_%s.jpeg", p$scan, tools::file_path_sans_ext(p$file)))
      overlay_page(file.path(folder, p$scan, "pages", p$file), p, cfg, layouts, out)
      full <- file.path(p$scan, "pages", p$file)
      sprintf(paste0('<figure><a href="%s"><img src="%s" loading="lazy"></a><figcaption>%s p%d &middot; ',
                     '<span class="%s">%s</span> &middot; %d/4 corner markers &middot; ',
                     '<a href="%s">full resolution</a></figcaption></figure>'),
              html_escape(full), html_escape(file.path(basename(img_dir), basename(out))),
              html_escape(p$file), p$page_no, if (p$registered) "ok" else "bad",
              if (is.finite(p$error)) sprintf("off by %g px (limit %g)", p$error, tol)
              else "could not be located", p$markers, html_escape(full))
    }, character(1))
    z <- sheets$zid[i]
    enrolled <- if (!length(names_of)) "" else if (!is.na(names_of[z])) names_of[[z]] else "<b>not on class list</b>"
    sug <- suggested[[paste(sheets$scan[i], sheets$files[i])]]
    sprintf(paste0('<section class="card"><h3><label><input type="checkbox"> %s</label></h3>',
                   '<p class="why">%s</p><table><tr><th>Scan</th><td>%s</td></tr>',
                   '<tr><th>zID read</th><td><code>%s</code> %s</td></tr>%s',
                   '<tr><th>Answers</th><td><code>%s</code> &middot; score %s &middot; version %s</td></tr>',
                   '</table><div class="pages">%s</div></section>'),
            html_escape(paste(sheets$scan[i], sheets$files[i])), why,
            html_escape(sheets$scan[i]), html_escape(z), enrolled,
            if (is.null(sug)) "" else sprintf("<tr><th>Suggested</th><td>%s</td></tr>", html_escape(sug)),
            html_escape(sheets$answers[i]), sheets$score[i], html_escape(sheets$version[i]),
            paste(imgs, collapse = ""))
  }
  section <- function(title, blurb, idx, why) {
    if (!length(idx)) return(sprintf("<h2>%s</h2><p>None.</p>", title))
    message("Spot check: ", title, " (", length(idx), ")")
    paste0(sprintf("<h2>%s <small>(%d)</small></h2><p>%s</p>", title, length(idx), blurb),
           paste(vapply(idx, card, character(1), why = why), collapse = "\n"))
  }

  ok <- reg$registered
  summary <- sprintf(paste0(
    "<p>%d pages in %d sheets. Registration: %d pages passed (largest error %g px), ",
    "%d refused. Limit %g px.</p><table class='hist'><tr><th>error (px)</th>%s</tr>",
    "<tr><th>pages</th>%s</tr></table>"),
    nrow(reg), nrow(sheets), sum(ok), if (any(ok)) max(reg$error[ok]) else NA, sum(!ok), tol,
    paste(sprintf("<td>%s</td>", c(0:8, "none")), collapse = ""),
    paste(sprintf("<td>%d</td>", c(vapply(0:8, function(e) sum(reg$error == e), integer(1)),
                                   sum(!is.finite(reg$error)))), collapse = ""))

  body <- paste(
    section("Refused by the registration check",
            "Each of these should be visibly crooked or shifted, with red boxes off the printed text. None of their answers were read: read them from the scan and record them in the review file.",
            refused, "Refused: confirm it is crooked; read the answers by hand."),
    section("Passed, closest to the limit",
            "The riskiest passes. Every box should sit on its printed text and every blue ring on a pen mark.",
            borderline, "Passed near the limit: confirm the boxes and rings line up."),
    section("zID not on the class list",
            "Read the zID bubbles and the written name, and check them against the class list.",
            off_roster, "zID not enrolled: check the bubbles and name."),
    section("Random clean sheets",
            "Sheets that went to the upload with nothing flagged. The blue rings should match the pen marks exactly, and the name written on the sheet should match the enrolled name.",
            random, "Random: confirm the zID and every answer."),
    sep = "\n")
  writeLines(spot_check_html(summary, body), output)
  message("Spot check written: ", output)
  invisible(reg)
}

# page_registration: registration error for every page in a scan folder,
# cached in registration.csv (redone when the scan is marked again).
page_registration <- function(dir, cfg, layouts) {
  cache <- file.path(dir, "registration.csv")
  if (file.exists(cache) && file.mtime(cache) > file.mtime(file.path(dir, "sheets.csv"))) {
    out <- utils::read.csv(cache, stringsAsFactors = FALSE, colClasses = c(version = "character"))
    out$error[is.na(out$error)] <- Inf
    return(out)
  }
  seq_df <- utils::read.csv(file.path(dir, "scan_sequence.csv"), stringsAsFactors = FALSE)
  prog <- utils::read.csv(file.path(dir, "progress.csv"), stringsAsFactors = FALSE)
  m <- match(seq_df$file, basename(prog$file))
  version <- as.character(prog$exam_version[m])
  version[is.na(version)] <- as.character(seq_df$version[is.na(version)])
  message("Measuring registration: ", basename(dir), " (", nrow(seq_df), " pages)")
  rows <- lapply(seq_len(nrow(seq_df)), function(k) {
    v <- if (!is.na(version[k]) && version[k] %in% names(layouts)) version[k] else names(layouts)[1]
    page_no <- if (is.na(seq_df$page_no[k])) 1L else as.integer(seq_df$page_no[k])
    r <- register_page(file.path(dir, "pages", seq_df$file[k]), cfg, layouts[[v]], page_no)
    data.frame(scan = basename(dir), file = seq_df$file[k], page_no = page_no, version = v,
               registered = r$registered, error = r$error, markers = r$ctx$markers,
               angle = round(page_angle(r$ctx$map_xy), 2), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  utils::write.csv(transform(out, error = ifelse(is.finite(error), error, NA)), cache, row.names = FALSE)
  out
}

# overlay_page: the scan with the anchors, every bubble and the bubbles read
# drawn on through the page map, scaled down to a screen-sized JPEG.
overlay_page <- function(img_path, p, cfg, layouts, out, width = 1100) {
  layout <- layouts[[p$version]]
  r <- register_page(img_path, cfg, layout, p$page_no)
  ctx <- r$ctx
  scale <- ctx$w / 1654
  anchors <- registration_anchors(cfg, layout, p$page_no)
  off <- registration_offsets(ctx, r$blank, anchors, search = as.integer(round(8 * scale)))
  read <- read_answers_cv(img_path, cfg, layout, p$page_no, reg = r)
  rad <- ctx$w * 7 / PAGE_W
  ring <- function(u, v, col, lwd) {
    pt <- ctx$map_xy(u, v)
    th <- seq(0, 2 * pi, length.out = 33)
    graphics::lines(pt[["x"]] + rad * cos(th), pt[["y"]] + rad * sin(th), col = col, lwd = lwd)
  }
  inks_pick <- function(inks, rule) {
    cls <- classify_bubble_row(inks, rule)
    list(letter = sub("[*]$", "", cls$answer), uncertain = isTRUE(cls$uncertain))
  }

  img <- magick::image_draw(ctx$img)
  for (k in seq_len(nrow(anchors))) {
    a <- anchors[k, ]
    box <- ctx$map_xy(a[["u"]] + c(-1, 1, 1, -1) * a[["hu"]], a[["v"]] + c(-1, -1, 1, 1) * a[["hv"]])
    good <- all(is.finite(off[k, ])) && max(abs(off[k, ])) <= REGISTRATION_TOLERANCE_PX * scale
    col <- if (good) "#1a9850" else "#d73027"
    graphics::polygon(box[, "x"], box[, "y"], border = col, lwd = 4)
    graphics::text(min(box[, "x"]), min(box[, "y"]) - 4, adj = c(0, 0), col = col, cex = 2.2, font = 2,
                   labels = if (all(is.finite(off[k, ]))) sprintf("%+d,%+d", off[k, 1], off[k, 2]) else "not found")
  }
  for (q in names(read$inks)) {
    col_q <- layout$QUESTION_COL[[q]]; y <- layout$QUESTION_Y[[q]]
    pick <- inks_pick(read$inks[[q]], ANSWER_READ)
    for (L in cfg$options) {
      x <- layout$ANSWER_X[[col_q]][[L]]
      if (L == pick$letter) ring(x, y, if (pick$uncertain) "#fc8d59" else "#2166ac", 6)
      else ring(x, y, "#999999", 1.5)
    }
  }
  if (p$page_no == 1L) {
    grid <- load_id_grid_from_form(form_pdf_for(layout), cfg)
    if (!is.null(grid)) for (ci in names(grid$x)) {
      blank <- blank_form_ctx(form_pdf_for(layout), 1L, ctx$w)
      inks <- vapply(names(grid$y), function(d) bubble_score(ctx, blank, grid$x[[ci]], grid$y[[d]], ZID_READ), numeric(1))
      names(inks) <- names(grid$y)
      pick <- inks_pick(inks, ZID_READ)
      if (pick$letter %in% names(grid$y)) ring(grid$x[[ci]], grid$y[[pick$letter]], if (pick$uncertain) "#fc8d59" else "#2166ac", 5)
    }
  }
  grDevices::dev.off()
  img <- magick::image_resize(img, sprintf("%dx", width))
  magick::image_write(img, out, format = "jpeg", quality = 80)
  invisible(out)
}

# roster_names: zID -> "First Last" from a Moodle grades export.
roster_names <- function(roster) {
  if (is.null(roster)) return(character(0))
  r <- utils::read.csv(roster, stringsAsFactors = FALSE, check.names = FALSE)
  stats::setNames(paste(r[["First name"]], r[["Last name"]]), tolower(r[["Username"]]))
}

# review_suggestions: "scan files" -> suggested zID and name, from any
# suggested_zid / suggested_name columns added to the review file.
review_suggestions <- function(review) {
  if (is.null(review) || !file.exists(review)) return(list())
  r <- utils::read.csv(review, stringsAsFactors = FALSE, colClasses = "character")
  if (!"suggested_zid" %in% names(r)) return(list())
  has <- nzchar(r$suggested_zid)
  nm <- if ("suggested_name" %in% names(r)) r$suggested_name else ""
  stats::setNames(as.list(trimws(paste(r$suggested_zid, nm)[has])),
                  paste(r$scan, r$files)[has])
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

spot_check_html <- function(summary, body) {
  paste0('<!doctype html><html><head><meta charset="utf-8"><title>Spot check</title><style>
body{font:15px/1.4 -apple-system,system-ui,sans-serif;margin:24px auto;max-width:1400px;padding:0 16px;background:#fff;color:#222}
h2{margin-top:40px;border-bottom:1px solid #ccc}.card{border:1px solid #ddd;border-radius:6px;padding:12px;margin:16px 0}
.card h3{margin:0;font-size:15px}.why{color:#555;margin:4px 0}table{border-collapse:collapse;margin:6px 0}
th,td{text-align:left;padding:2px 10px 2px 0}.hist td,.hist th{border:1px solid #ddd;padding:2px 8px;text-align:center}
.pages{display:flex;gap:12px;flex-wrap:wrap}figure{margin:0;flex:1 1 480px}img{width:100%;border:1px solid #ccc}
figcaption{font-size:13px;color:#555}.ok{color:#1a9850;font-weight:600}.bad{color:#d73027;font-weight:600}
.key span{display:inline-block;width:14px;height:14px;border-radius:50%;vertical-align:middle;margin:0 4px 0 12px}
</style></head><body><h1>Spot check</h1>
<p class="key">Boxes: printed anchors where the page map puts them (<b class="ok">within limit</b> / <b class="bad">not</b>, with the offset in px).
Rings: <span style="border:1px solid #999"></span>every bubble <span style="border:3px solid #2166ac"></span>read as filled
<span style="border:3px solid #fc8d59"></span>read, uncertain. Click an image for the full-size scan. Tick each sheet once checked.</p>',
         summary, body, "</body></html>")
}
