# paper.R -- render generated question-paper markdown to printable PDFs.

# A figure line sits between a question's stem and its options:
#   ![caption](path/to/figure.png){width=0.7}
# `width` is a fraction of the text width (default 0.8). Paths are relative to
# the markdown file that contains them.
figure_line_regex <- "^!\\[(.*)\\]\\(([^)]+)\\)(?:\\{width=([0-9.]+)\\})?\\s*$"

figure_tex <- function(fig, max_height = "7cm") {
  if (is.null(fig)) return(character(0))
  max_height <- max_height %||% "7cm"
  c(
    "\\begin{center}",
    sprintf("\\includegraphics[width=%.2f\\linewidth, height=%s, keepaspectratio]{%s}",
            fig$width, max_height, fig$path),
    if (nzchar(fig$caption)) sprintf("\\\\[2pt]{\\footnotesize %s}", tex_escape_math(fig$caption)),
    "\\end{center}"
  )
}

has_figures <- function(blocks) {
  any(vapply(blocks, function(q) !is.null(q$figure), logical(1)))
}

latex_question_paper <- function(cfg, blocks, version) {
  question_tex <- unlist(lapply(blocks, function(q) {
    opts <- q$options[tolower(cfg$options)]
    if (any(is.na(opts))) {
      stop("Question ", q$number, " is missing one or more options.", call. = FALSE)
    }
    c(
      sprintf("\\textbf{Q%d.} %s\\par", q$number, tex_escape_math(q$question)),
      figure_tex(q$figure, cfg$figure_max_height),
      "\\begin{enumerate}[label=\\alph*., leftmargin=1.8em]",
      sprintf("  \\item %s", tex_escape_math(opts)),
      "\\end{enumerate}",
      "\\vspace{4pt}"
    )
  }), use.names = FALSE)

  c(
    "\\documentclass[11pt, a4paper]{article}",
    "\\usepackage[a4paper, left=1.8cm, right=1.8cm, top=1.4cm, bottom=1.4cm]{geometry}",
    "\\usepackage{fontspec}",
    "\\setmainfont{Helvetica Neue}",
    "\\usepackage{enumitem}",
    if (has_figures(blocks)) "\\usepackage{graphicx}",
    "\\setlength{\\parindent}{0pt}",
    "\\setlength{\\parskip}{2pt}",
    "\\begin{document}",
    sprintf("\\noindent{\\LARGE\\bfseries %s}\\hfill{\\large\\bfseries Version %s}\\\\[1pt]",
            tex_escape(cfg$title), tex_escape(version)),
    sprintf("{\\normalsize %s}\\\\[2pt]", tex_escape(cfg$subtitle)),
    "\\rule{\\linewidth}{1pt}",
    "\\vspace{6pt}",
    question_tex,
    "\\end{document}"
  )
}

#' Render generated question papers to PDF
#'
#' Converts `questions_v*.md` files from [generate_versions()] into a plain
#' flowing question-only document per version -- no bubbles, no registration
#' marks, no QR, because this document is never scanned. Paired with
#' [make_bubblesheet()], which renders the separate, scannable bubble-only
#' answer sheet. Unlike the answer sheet, this document has no page cap: it can
#' run to as many pages as the quiz needs.
#'
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param outdir Directory containing `questions_v*.md`.
#' @return Invisibly, the written `.tex` and `.pdf` paths.
#' @export
render_papers <- function(config = default_config_path(), outdir = "output") {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  if (!dir.exists(outdir)) stop("Output directory not found: ", outdir, call. = FALSE)

  written <- character(0)
  for (v in cfg$valid_versions) {
    md <- file.path(outdir, sprintf("questions_v%s.md", v))
    if (!file.exists(md)) {
      stop("Question version not found: ", md,
           "\nRun `bubblequiz versions` first.", call. = FALSE)
    }
    blocks <- parse_question_blocks(md, cfg)
    tex_path <- file.path(outdir, sprintf("questions_v%s.tex", v))
    writeLines(latex_question_paper(cfg, blocks, v), tex_path)
    written <- c(written, tex_path)
    message("Wrote: ", tex_path)
    written <- c(written, render_xelatex(tex_path, outdir))
  }

  invisible(written)
}

parse_question_blocks <- function(path, cfg) {
  lines <- readLines(path, warn = FALSE)
  base_dir <- dirname(path)
  opts <- tolower(cfg$options)
  blocks <- list()
  current <- NULL

  flush <- function() {
    if (!is.null(current)) blocks[[length(blocks) + 1L]] <<- current
  }

  for (line in lines) {
    if (grepl("^<!--\\s*Answer\\s+", line)) next
    m <- regexec("^\\*\\*Question\\s+([0-9]+)(?:\\s+\\[[^]]+\\])?:\\*\\*\\s*(.*)$", line)
    hit <- regmatches(line, m)[[1]]
    if (length(hit) > 0) {
      flush()
      current <- list(
        number = as.integer(hit[2]),
        question = hit[3],
        options = stats::setNames(rep(NA_character_, length(opts)), opts)
      )
      next
    }

    if (is.null(current) || !nzchar(trimws(line))) next
    fig_hit <- regmatches(line, regexec(figure_line_regex, line, perl = TRUE))[[1]]
    if (length(fig_hit) > 0) {
      if (!is.null(current$figure)) {
        stop("Question ", current$number, " has more than one figure line.", call. = FALSE)
      }
      fig_path <- if (grepl("^(/|[A-Za-z]:)", fig_hit[3])) fig_hit[3] else file.path(base_dir, fig_hit[3])
      if (!file.exists(fig_path)) {
        stop("Question ", current$number, ": figure not found: ", fig_hit[3],
             " (looked in ", normalizePath(base_dir), ")", call. = FALSE)
      }
      current$figure <- list(
        caption = trimws(fig_hit[2]),
        path    = normalizePath(fig_path),
        width   = if (nzchar(fig_hit[4])) min(as.numeric(fig_hit[4]), 1) else 0.8
      )
      next
    }
    opt_match <- regexec(paste0("^([", paste(opts, collapse = ""), "])\\.\\s+(.*)$"), line)
    opt_hit <- regmatches(line, opt_match)[[1]]
    if (length(opt_hit) > 0) {
      current$options[[tolower(opt_hit[2])]] <- opt_hit[3]
    } else {
      current$question <- paste(current$question, trimws(line))
    }
  }
  flush()

  want <- as.integer(cfg$questions)
  got <- vapply(blocks, `[[`, integer(1), "number")
  if (!identical(sort(got), want)) {
    stop("Question paper ", basename(path), " does not match config questions.",
         "\n  In config: ", paste(want, collapse = ", "),
         "\n  In paper:  ", paste(sort(got), collapse = ", "),
         call. = FALSE)
  }

  blocks[match(want, got)]
}

latex_inline_quiz <- function(cfg, blocks, version) {
  bubble_row <- paste0(sprintf("\\bub{%s}", cfg$options), collapse = "")
  qr_payload <- sprintf("bubblequiz|course=%s|version=%s|questions=%s",
                        cfg$course, version, paste(cfg$questions, collapse = ","))

  question_tex <- unlist(lapply(blocks, function(q) {
    opts <- q$options[tolower(cfg$options)]
    if (any(is.na(opts))) {
      stop("Question ", q$number, " is missing one or more options.", call. = FALSE)
    }
    c(
      "\\begin{samepage}",
      sprintf("\\questionblock{%d}{%s}", q$number, tex_escape_math(q$question)),
      figure_tex(q$figure, cfg$figure_max_height),
      sprintf("\\begin{enumerate}[label=\\alph*., leftmargin=1.4em, itemsep=%s, topsep=2pt]",
              cfg$spacing$option %||% "1pt"),
      sprintf("  \\item %s", tex_escape_math(opts)),
      "\\end{enumerate}",
      sprintf("\\answerline{%d}{%s}", q$number, bubble_row),
      sprintf("\\vspace{%s}", cfg$spacing$question %||% "5pt"),
      "\\end{samepage}"
    )
  }), use.names = FALSE)

  c(
    "\\documentclass[10pt, a4paper]{article}",
    "\\usepackage[a4paper, left=1.4cm, right=1.4cm, top=0.9cm, bottom=0.9cm]{geometry}",
    "\\usepackage{fontspec}",
    "\\setmainfont{Helvetica Neue}",
    "\\usepackage{tikz}",
    "\\usepackage{eso-pic}",
    "\\usepackage{lastpage}",
    "\\usepackage{refcount}",
    "\\usepackage{xcolor}",
    "\\usepackage{enumitem}",
    if (has_figures(blocks)) "\\usepackage{graphicx}",
    "\\usepackage[nolinks]{qrcode}",
    "\\pagestyle{empty}",
    "\\setlength{\\parindent}{0pt}",
    "\\setlength{\\parskip}{2pt}",
    "\\newcommand{\\bub}[1]{\\begin{tikzpicture}[baseline=-0.6ex]\\draw[line width=1.2pt](0,0) circle (8pt);\\node[font=\\fontsize{7.2}{7.2}\\selectfont\\bfseries] at (0,0){#1};\\end{tikzpicture}\\hspace{4pt}}",
    sprintf("\\newcommand{\\questionblock}[2]{\\vspace{%s}\\textbf{Q#1.} #2\\par}",
            cfg$spacing$stem %||% "4pt"),
    # Fixed-width label. Left to its natural width, "Answer Q10" is wider than
    # "Answer Q1", which shifts the bubble row right on any page carrying
    # double-digit questions -- calibration then refuses the form because the
    # columns no longer line up across pages.
    "\\newcommand{\\answerline}[2]{\\makebox[5.9em][l]{\\textbf{Answer Q#1}}#2\\par}",
    # Per-page QR payload. The page and page-count fields are expanded at
    # shipout, so every page identifies itself: a scanned stack can be checked
    # for missing pages and reordered without relying on scan order. The
    # version= field keeps the same spelling the marker already parses.
    sprintf("\\newcommand{\\bqpagepayload}{%s|page=\\the\\value{page}|pages=\\getpagerefnumber{LastPage}}",
            tex_escape(qr_payload)),
    "\\newcommand{\\bqpageqr}{\\expanded{\\noexpand\\qrcode[height=1.3cm]{\\bqpagepayload}}}",
    # Corner fiducials go in the shipout background so they are drawn on EVERY
    # page. Emitting them as body content puts them on page 1 only, which leaves
    # later pages of a multi-page form with nothing for the marker to orient on.
    "\\AddToShipoutPictureBG{%",
    "\\begin{tikzpicture}[remember picture, overlay]",
    "  \\fill[black] ([xshift= 3mm, yshift= -3mm]current page.north west) rectangle ++( 5mm, -5mm);",
    "  \\fill[black] ([xshift=-8mm, yshift= -3mm]current page.north east) rectangle ++( 5mm, -5mm);",
    "  \\fill[black] ([xshift= 3mm, yshift=  3mm]current page.south west) rectangle ++( 5mm,  5mm);",
    "  \\fill[black] ([xshift=-8mm, yshift=  3mm]current page.south east) rectangle ++( 5mm,  5mm);",
    # Page number on every page, plus a name line on the later pages only.
    # Page 1 already carries the full name/zID bubble header block, so repeating it
    # there would give the student two name slots on the same page; pages after
    # the first have no header, and need somewhere to write a name so a
    # separated sheet can still be attributed.
    "  \\node[anchor=south west, inner sep=0pt, font=\\footnotesize] at ([xshift=11mm, yshift=4mm]current page.south west) {\\ifnum\\value{page}>1 \\textbf{Name}~\\underline{\\hspace{55mm}}\\quad\\fi Page \\thepage\\ of \\pageref{LastPage}};",
    # Version/page QR in the bottom-right of every page, clear of the corner
    # squares, so a scanned stack can be matched without scan-order assumptions.
    "  \\node[anchor=south east, inner sep=0pt] at ([xshift=-12mm, yshift=3mm]current page.south east) {\\bqpageqr};",
    "\\end{tikzpicture}}",
    "\\begin{document}",
    # No title block: the zID bubble grid is the one thing every student must
    # fill in correctly for the paper to be attributable, so it sits at the very
    # top of the page rather than below a banner. Version and date move to the
    # left, where the eye starts; the page QR and the corner fiducials still
    # identify the paper for the marker.
    "\\begin{minipage}[t]{0.40\\linewidth}",
    sprintf("\\colorbox{black}{\\textcolor{white}{\\large\\bfseries\\quad Version %s\\quad}}\\\\[3pt]",
            tex_escape(version)),
    sprintf("{\\footnotesize %s}\\\\[10pt]", tex_escape(cfg$date)),
    "\\textbf{Name}\\quad\\underline{\\hspace{0.68\\linewidth}}",
    "\\end{minipage}\\hfill",
    "\\begin{minipage}[t]{0.56\\linewidth}",
    sprintf("\\textbf{Fill %s digit bubbles:}\\\\[-2pt]", tex_escape(cfg$id$label)),
    sprintf("\\begin{tikzpicture}[x=0.82cm, y=-0.50cm]\\foreach \\col in {0,...,%d}{\\foreach \\d in {0,...,9}{\\draw[line width=0.8pt](\\col, \\d) circle (0.19cm);\\node[font=\\fontsize{6.4}{6.4}\\selectfont] at (\\col, \\d) {\\d};}}\\end{tikzpicture}",
            cfg$id$digits - 1L),
    "\\end{minipage}",
    "\\vspace{3pt}\\rule{\\linewidth}{1.2pt}",
    # Optional instruction line, printed once under the header. Inline forms
    # print nothing unless exam.yml sets instructions:, so existing papers are
    # unchanged.
    if (!is.null(cfg$instructions)) {
      c(sprintf("\\vspace{2pt}\\textbf{%s}\\par", tex_escape(cfg$instructions)),
        "\\vspace{2pt}\\rule{\\linewidth}{0.6pt}")
    },
    question_tex,
    "\\end{document}"
  )
}

#' Render inline quiz forms
#'
#' Creates one printable/scannable PDF per version. Each form contains the
#' randomized questions and a bubble row directly after each question. Inline
#' forms require `layout.columns: 1` so calibration and marking map every row
#' to the question printed immediately above it.
#'
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param outdir Directory containing `questions_v*.md`.
#' @param render Compile generated `.tex` files to PDF with xelatex.
#' @return Invisibly, the generated paths.
#' @export
make_quiz_forms <- function(config = default_config_path(),
                            outdir = "output",
                            render = TRUE) {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  if (cfg$n_cols != 1L) {
    stop("Inline quiz forms require `layout.columns: 1` in exam.yml.",
         call. = FALSE)
  }
  if (!dir.exists(outdir)) stop("Output directory not found: ", outdir, call. = FALSE)

  written <- character(0)
  for (v in cfg$valid_versions) {
    md <- file.path(outdir, sprintf("questions_v%s.md", v))
    if (!file.exists(md)) {
      stop("Question version not found: ", md,
           "\nRun `bubblequiz versions` first.", call. = FALSE)
    }
    blocks <- parse_question_blocks(md, cfg)
    tex_path <- file.path(outdir, sprintf("quizform_v%s.tex", v))
    writeLines(latex_inline_quiz(cfg, blocks, v), tex_path)
    written <- c(written, tex_path)
    message("Wrote: ", tex_path)
    if (render) {
      pdf_path <- render_xelatex(tex_path, outdir)
      written <- c(written, pdf_path)
      enforce_page_cap(pdf_path, v)
    }
  }
  invisible(written)
}

#' Combine all quiz form versions into one print PDF
#'
#' The output contains each version as a complete block, in version order. For
#' double-sided printing, each multi-page version stays together before the next
#' version begins.
#'
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param outdir Directory containing `quizform_v*.pdf`.
#' @param output Output PDF path.
#' @return Invisibly, the output PDF path.
#' @export
combine_quiz_forms <- function(config = default_config_path(),
                               outdir = "output",
                               output = file.path(outdir, "quizforms_all_versions.pdf")) {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  pdfs <- file.path(outdir, sprintf("quizform_v%s.pdf", cfg$valid_versions))
  missing <- pdfs[!file.exists(pdfs)]
  if (length(missing) > 0) {
    stop("Missing quiz form PDF(s): ", paste(missing, collapse = ", "),
         "\nRun `bubblequiz forms` first.", call. = FALSE)
  }
  qpdf::pdf_combine(pdfs, output = output)
  message("Wrote: ", output)
  invisible(output)
}
