# paper.R -- render generated question-paper markdown to printable PDFs.

paper_wrapper <- function(cfg, source_md, style_file) {
  body <- readLines(source_md, warn = FALSE)
  c(
    "---",
    sprintf("title: \"%s\"", gsub("\"", "\\\\\"", cfg$title)),
    "format:",
    "  pdf:",
    "    pdf-engine: xelatex",
    sprintf("    include-in-header: %s", basename(style_file)),
    "---",
    "",
    body
  )
}

#' Render generated question papers to PDF
#'
#' Converts `questions_v*.md` files from [generate_versions()] into PDFs using
#' Quarto and the package's compact exam style.
#'
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param outdir Directory containing `questions_v*.md`.
#' @return Invisibly, the rendered PDF paths.
#' @export
render_papers <- function(config = default_config_path(), outdir = "output") {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  if (!dir.exists(outdir)) stop("Output directory not found: ", outdir, call. = FALSE)

  md_files <- list.files(outdir, pattern = "^questions_v[0-9]+\\.md$",
                         full.names = TRUE)
  if (length(md_files) == 0) {
    stop("No generated question files found in ", outdir,
         "\nRun `bubblequiz versions` first.", call. = FALSE)
  }
  if (Sys.which("quarto") == "") {
    stop("quarto was not found on PATH; install Quarto to render paper PDFs.",
         call. = FALSE)
  }

  style_src <- bq_file("templates", "style.tex")
  style_dst <- file.path(outdir, "style.tex")
  file.copy(style_src, style_dst, overwrite = TRUE)

  rendered <- character(0)
  old_wd <- setwd(normalizePath(outdir))
  on.exit(setwd(old_wd), add = TRUE)

  for (md in md_files) {
    base <- tools::file_path_sans_ext(basename(md))
    qmd <- paste0(base, ".qmd")
    pdf <- paste0(base, ".pdf")
    writeLines(paper_wrapper(cfg, basename(md), style_dst), qmd)

    message("Rendering: ", file.path(outdir, qmd))
    status <- system2("quarto", c("render", qmd, "--to", "pdf"), stdout = FALSE)
    if (!identical(status, 0L)) {
      stop("quarto failed while rendering ", qmd, call. = FALSE)
    }
    if (!file.exists(pdf)) {
      stop("quarto did not write expected PDF: ", file.path(outdir, pdf),
           call. = FALSE)
    }
    rendered <- c(rendered, file.path(outdir, pdf))
    message("Wrote: ", file.path(outdir, pdf))
  }

  invisible(rendered)
}

parse_question_blocks <- function(path, cfg) {
  lines <- readLines(path, warn = FALSE)
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
  id_blanks <- paste0(rep("\\underline{\\hspace{1.6em}}", cfg$id$digits), collapse = "\\,")
  id_example <- paste0(cfg$id$prefix,
    substr(paste(rep("1234567890", 2), collapse = ""), 1, cfg$id$digits))

  question_tex <- unlist(lapply(blocks, function(q) {
    opts <- q$options[tolower(cfg$options)]
    if (any(is.na(opts))) {
      stop("Question ", q$number, " is missing one or more options.", call. = FALSE)
    }
    c(
      "\\begin{samepage}",
      sprintf("\\questionblock{%d}{%s}", q$number, tex_escape(q$question)),
      sprintf("\\begin{enumerate}[label=\\alph*., leftmargin=1.4em, itemsep=%s, topsep=2pt]",
              cfg$spacing$option %||% "1pt"),
      sprintf("  \\item %s", tex_escape(opts)),
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
    "\\usepackage[nolinks]{qrcode}",
    "\\pagestyle{empty}",
    "\\setlength{\\parindent}{0pt}",
    "\\setlength{\\parskip}{2pt}",
    "\\newcommand{\\bub}[1]{\\begin{tikzpicture}[baseline=-0.6ex]\\draw[line width=1.2pt](0,0) circle (8pt);\\node[font=\\fontsize{7.2}{7.2}\\selectfont\\bfseries] at (0,0){#1};\\end{tikzpicture}\\hspace{4pt}}",
    sprintf("\\newcommand{\\questionblock}[2]{\\vspace{%s}\\textbf{Q#1.} #2\\par}",
            cfg$spacing$stem %||% "4pt"),
    "\\newcommand{\\answerline}[2]{\\textbf{Answer Q#1}\\quad #2\\par}",
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
    # A name line in the bottom-left of every page, mirroring the QR. Page 1
    # also has the full name/zID header block; repeating it means a separated
    # sheet can still be attributed to a student.
    "  \\node[anchor=south west, inner sep=0pt, font=\\footnotesize] at ([xshift=11mm, yshift=4mm]current page.south west) {\\textbf{Name}~\\underline{\\hspace{55mm}}\\quad Page \\thepage\\ of \\pageref{LastPage}};",
    # Repeat the version QR in the bottom-right of every page, clear of the
    # corner squares. Page 1 carries the header QR as well; the redundancy means
    # a torn or over-cropped page can still be matched to its version.
    "  \\node[anchor=south east, inner sep=0pt] at ([xshift=-12mm, yshift=3mm]current page.south east) {\\bqpageqr};",
    "\\end{tikzpicture}}",
    "\\begin{document}",
    "\\begin{minipage}[t]{0.70\\linewidth}",
    sprintf("{\\LARGE\\bfseries %s}\\\\[1pt]", tex_escape(cfg$title)),
    sprintf("{\\normalsize %s}", tex_escape(cfg$subtitle)),
    "\\end{minipage}\\hfill",
    "\\begin{minipage}[t]{0.28\\linewidth}\\raggedleft",
    "\\begin{minipage}[t]{0.54\\linewidth}\\raggedleft",
    sprintf("\\colorbox{black}{\\textcolor{white}{\\large\\bfseries\\quad Version %s\\quad}}\\\\[2pt]",
            tex_escape(version)),
    sprintf("{\\footnotesize %s}", tex_escape(cfg$date)),
    "\\end{minipage}\\hspace{0.7em}",
    "\\begin{minipage}[t]{0.31\\linewidth}",
    sprintf("\\qrcode[height=1.05cm]{%s}", tex_escape(qr_payload)),
    "\\end{minipage}",
    "\\end{minipage}\\\\[2pt]",
    "\\rule{\\linewidth}{1.2pt}",
    "\\begin{minipage}[t]{0.40\\linewidth}",
    "\\vspace{2pt}",
    "\\textbf{Name}\\quad\\underline{\\hspace{0.74\\linewidth}}\\\\[8pt]",
    sprintf("\\textbf{%s}\\quad {\\small e.g. %s}\\\\[3pt]",
            tex_escape(cfg$id$label), tex_escape(id_example)),
    sprintf("%s\\,%s", tex_escape(cfg$id$prefix), id_blanks),
    "\\end{minipage}\\hfill",
    "\\begin{minipage}[t]{0.56\\linewidth}",
    "\\vspace{2pt}",
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
    if (render) written <- c(written, render_xelatex(tex_path, outdir))
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
