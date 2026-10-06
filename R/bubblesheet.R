# bubblesheet.R -- generate the printable OMR answer sheet from the exam config.
#
# The static half of the sheet (page geometry, bubble macros, registration
# marks, header, ID grid) ships with the package as
# inst/templates/bubblesheet_preamble.tex. This file emits the per-exam half:
# the \def's the preamble needs, then one section bar + answer-row tabular per
# section, laid out exactly as load_exam_config() says.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
tex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  for (ch in c("&", "%", "$", "#", "_", "{", "}")) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  x
}

# Like tex_escape(), but passes `$...$` spans through as LaTeX math. Pandoc's
# rule decides what counts as math, so prices stay literal: the opening `$`
# must be followed by a non-space, the closing `$` preceded by a non-space and
# not followed by a digit. `\$` is a literal dollar sign. Inside math only `%`
# and `#` are escaped (a stray `%` would comment out the rest of the line).
# Anything that does not pair up is escaped exactly as tex_escape() would.
tex_escape_math <- function(x) {
  x <- as.character(x)
  math_re <- "(?<!\\\\)\\$(?=[^\\s$])(?:[^$\\\\]|\\\\.)*?(?<=[^\\s\\\\])\\$(?![0-9])"
  escape_outside <- function(txt) {
    txt <- gsub("\\$", "\001", txt, fixed = TRUE)
    txt <- tex_escape(txt)
    gsub("\001", "\\$", txt, fixed = TRUE)
  }
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    m <- gregexpr(math_re, s, perl = TRUE)[[1]]
    if (m[1] == -1L) return(escape_outside(s))
    starts <- as.integer(m)
    ends   <- starts + attr(m, "match.length") - 1L
    out <- character(0)
    pos <- 1L
    for (k in seq_along(starts)) {
      if (starts[k] > pos) out <- c(out, escape_outside(substr(s, pos, starts[k] - 1L)))
      span <- substr(s, starts[k], ends[k])
      span <- gsub("(?<!\\\\)([%#])", "\\\\\\1", span, perl = TRUE)
      out <- c(out, span)
      pos <- ends[k] + 1L
    }
    if (pos <= nchar(s)) out <- c(out, escape_outside(substr(s, pos, nchar(s))))
    paste(out, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# \bub{A}\bub{B}... for one answer row
bubbles_tex <- function(cfg) {
  paste0(sprintf("\\bub{%s}", cfg$options), collapse = "")
}

# Section header right-hand summary: "Q1-Q10   10 x 2 marks = 20 marks"
section_summary <- function(s, cfg) {
  parts <- character(0)
  n <- length(s$questions)
  if (n > 0) {
    parts <- c(parts, sprintf("%d $\\times$ %s marks", n, fmt_marks(s$marks_each)))
  }
  for (e in s$essays) {
    parts <- c(parts, sprintf("%s marks", fmt_marks(e$marks)))
  }
  total <- n * as.numeric(s$marks_each) +
    sum(vapply(s$essays, function(e) as.numeric(e$marks %||% 0), numeric(1)))
  lhs <- paste(parts, collapse = " + ")
  # An essay-only section already states its total; don't print "10 marks = 10 marks".
  if (length(parts) == 1 && n == 0) {
    sprintf("%s \\quad %s", question_range_label(s, cfg), lhs)
  } else {
    sprintf("%s \\quad %s = %s marks", question_range_label(s, cfg), lhs, fmt_marks(total))
  }
}

fmt_marks <- function(x) {
  if (is.null(x) || is.na(x)) return("?")
  x <- as.numeric(x)
  if (x == round(x)) format(round(x)) else format(x)
}

question_range_label <- function(s, cfg) {
  all_q <- sort(c(s$questions, vapply(s$essays, `[[`, integer(1), "number")))
  if (length(all_q) == 0) return("")
  if (length(all_q) == 1) sprintf("Q%d", all_q)
  else sprintf("Q%d--Q%d", min(all_q), max(all_q))
}

# ---------------------------------------------------------------------------
# Instructions paragraph
# ---------------------------------------------------------------------------
default_instructions <- function(cfg) {
  opts_range <- paste0(cfg$options[1], "--", cfg$options[length(cfg$options)])
  txt <- paste0(
    "\\textbf{Instructions:} ",
    "Use a \\textbf{black or blue pen}. ",
    "Fill each bubble \\textbf{completely and clearly}. ",
    "To correct a mistake, \\textbf{cross out} the wrong bubble and fill the correct one. ",
    "Mark \\textbf{one answer per question only}."
  )
  if (length(cfg$essay_questions) > 0) {
    qs <- paste0("Q", cfg$essay_questions, collapse = " and ")
    verb <- if (length(cfg$essay_questions) == 1) "is an essay question" else "are essay questions"
    txt <- paste0(txt, sprintf(
      " \\textit{%s %s --- write those in the question booklet, not here.}", qs, verb))
  }
  txt
}

# ---------------------------------------------------------------------------
# Body: one section bar + tabular per section
# ---------------------------------------------------------------------------
build_body <- function(cfg) {
  bub <- bubbles_tex(cfg)
  out <- character(0)

  for (s in cfg$sections) {
    items <- c(s$questions, vapply(s$essays, `[[`, integer(1), "number"))
    if (length(items) == 0) next

    essay_marks <- stats::setNames(
      vapply(s$essays, function(e) as.numeric(e$marks %||% NA), numeric(1)),
      vapply(s$essays, `[[`, integer(1), "number")
    )

    out <- c(out,
      sprintf("%%%%-- Section %s ", s$id),
      sprintf("\\sectionbar{Section %s --- %s}{%s}",
              tex_escape(s$id), tex_escape(s$title), section_summary(s, cfg)),
      "",
      sprintf("\\begin{tabular}{%s}",
              paste0("@{}", paste(rep("l", cfg$n_cols), collapse = "@{\\hspace{1.8cm}}"), "@{}"))
    )

    rows <- layout_section(items, cfg$n_cols)
    for (i in seq_along(rows)) {
      r <- rows[[i]]
      cells <- vapply(cfg$col_names, function(col) {
        q <- r[[col]]
        if (is.null(q)) return("")
        if (q %in% cfg$questions) {
          sprintf("\\qrow{%d}{%s}", q, bub)
        } else {
          sprintf("\\erow{%d}{%s}", q, fmt_marks(essay_marks[[as.character(q)]]))
        }
      }, character(1))
      sep <- if (i < length(rows)) " \\\\[4pt]" else " \\\\"
      out <- c(out, paste0("  ", paste(cells, collapse = " & "), sep))
    }

    out <- c(out, "\\end{tabular}", "", "\\vspace{0.3cm}", "")
  }
  out
}

# ---------------------------------------------------------------------------
# Emit one .tex per version
# ---------------------------------------------------------------------------

#' Generate the printable bubble answer sheet
#'
#' Writes one LaTeX file per exam version into `outdir`, then compiles each to
#' PDF with xelatex (twice, so TikZ `remember picture` positions settle).
#'
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param outdir Output directory, created if needed.
#' @param render Compile the generated `.tex` to PDF with xelatex.
#' @return Invisibly, the paths written.
#' @export
make_bubblesheet <- function(config = default_config_path(),
                             outdir = "output",
                             render = TRUE) {
  cfg <- if (is.list(config)) config else load_exam_config(config)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  id_blanks <- paste0(
    rep("\\underline{\\hspace{1.6em}}", cfg$id$digits), collapse = "\\,")

  id_example <- paste0(cfg$id$prefix,
    substr(paste(rep("1234567890", 2), collapse = ""), 1, cfg$id$digits))

  duration_marks <- paste(
    c(if (nzchar(cfg$duration)) tex_escape(cfg$duration),
      if (!is.na(cfg$total_marks)) sprintf("%s marks total", fmt_marks(cfg$total_marks))),
    collapse = " \\; | \\; ")

  body <- build_body(cfg)
  # The preamble is copied next to the generated .tex so xelatex resolves it
  # regardless of where the course repo keeps its output directory.
  preamble_src <- bq_file("templates", "bubblesheet_preamble.tex")
  file.copy(preamble_src, file.path(outdir, "bubblesheet_preamble.tex"), overwrite = TRUE)

  written <- character(0)
  for (v in cfg$valid_versions) {
    qr_payload <- sprintf("bubblequiz|course=%s|version=%s|questions=%s",
                          cfg$course, v, paste(cfg$questions, collapse = ","))
    lines <- c(
      sprintf("%% GENERATED by bubblequiz::make_bubblesheet() from %s -- do not edit.",
              basename(cfg$path)),
      sprintf("\\def\\bqpagepayloadbase{%s}", tex_escape(qr_payload)),
      sprintf("\\def\\examversion{%s}", v),
      sprintf("\\def\\examtitle{%s}", tex_escape(cfg$title)),
      sprintf("\\def\\examdate{%s}", tex_escape(cfg$date)),
      sprintf("\\def\\examsubtitle{%s}", tex_escape(cfg$subtitle)),
      sprintf("\\def\\examdurationmarks{%s}", duration_marks),
      sprintf("\\def\\examinstructions{%s}", cfg$instructions %||% default_instructions(cfg)),
      sprintf("\\def\\idlabel{%s}", tex_escape(cfg$id$label)),
      sprintf("\\def\\idprefix{%s}", tex_escape(cfg$id$prefix)),
      sprintf("\\def\\idexample{%s\\textbf{%s}}", tex_escape(cfg$id$prefix),
              substr(id_example, nchar(cfg$id$prefix) + 1, nchar(id_example))),
      sprintf("\\def\\idlastcol{%d}", cfg$id$digits - 1L),
      sprintf("\\def\\idblanks{%s}", id_blanks),
      "\\input{bubblesheet_preamble.tex}",
      "",
      body,
      "\\vspace{6pt}",
      "\\noindent\\rule{\\linewidth}{0.6pt}",
      "\\vspace{3pt}",
      "",
      "\\end{document}"
    )
    tex_path <- file.path(outdir, sprintf("bubblesheet_v%s.tex", v))
    writeLines(lines, tex_path)
    written <- c(written, tex_path)
    message("Wrote: ", tex_path)

    if (render) {
      pdf_path <- render_xelatex(tex_path, outdir)
      written <- c(written, pdf_path)
      enforce_page_cap(pdf_path, v)
    }
  }

  message(sprintf("%d version(s), %d MCQ questions, %d bubble row(s).",
                  length(cfg$valid_versions), length(cfg$questions),
                  length(cfg$bubble_rows)))
  invisible(written)
}

#' Compile a LaTeX file to PDF with xelatex
#'
#' Runs xelatex twice: TikZ `remember picture` needs a second pass to place the
#' corner registration marks, and those marks are what the marker aligns to.
#'
#' @param tex_path Path to the `.tex` file.
#' @param outdir Directory for the PDF and intermediates.
#' @return Invisibly, the PDF path.
#' @export
render_xelatex <- function(tex_path, outdir = dirname(tex_path)) {
  tex_path <- normalizePath(tex_path, mustWork = TRUE)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  outdir <- normalizePath(outdir, mustWork = TRUE)
  job <- tools::file_path_sans_ext(basename(tex_path))
  args <- c("-interaction=nonstopmode",
            paste0("-output-directory=", outdir),
            paste0("-jobname=", job),
            basename(tex_path))
  wd <- setwd(dirname(tex_path))
  on.exit(setwd(wd), add = TRUE)

  for (pass in 1:2) {
    status <- system2("xelatex", args, stdout = FALSE, stderr = FALSE)
  }
  pdf_path <- file.path(outdir, paste0(job, ".pdf"))
  # nonstopmode carries on past errors and usually still writes a PDF -- a
  # missing font, say, gives a form with no text on it. So an error status
  # fails the render even when a PDF exists, and the log's errors are shown.
  if (!identical(as.integer(status), 0L) || !file.exists(pdf_path)) {
    log_path <- file.path(outdir, paste0(job, ".log"))
    if (file.exists(log_path)) {
      log <- readLines(log_path, warn = FALSE)
      errs <- grep("^!", log)
      shown <- if (length(errs)) unique(unlist(lapply(errs, function(i) i:min(i + 3L, length(log)))))
               else max(1L, length(log) - 24L):length(log)
      writeLines(log[shown])
    }
    stop("xelatex failed for ", tex_path, call. = FALSE)
  }
  for (ext in c(".aux", ".log")) unlink(file.path(outdir, paste0(job, ext)))
  message("Wrote: ", pdf_path)
  invisible(pdf_path)
}

# A scanned answer sheet is one physical piece of paper, printed duplex. A
# stapled multi-sheet packet is a pain to run through a scanner feeder, so a
# rendered answer sheet (inline quiz form, or a separate-mode bubble sheet) is
# capped at 2 pages -- front and back of one sheet -- not pages in general.
enforce_page_cap <- function(pdf_path, version) {
  n_pages <- pdftools::pdf_length(pdf_path)
  if (n_pages > 2L) {
    stop(sprintf(
      "Version %s rendered to %d pages (%s). Answer sheets are capped at 2 pages -- front and back of one physical sheet -- because a stapled multi-sheet packet is a pain to run through a scanner feeder. Shorten the quiz, or move question text onto a separate question paper (layout.columns > 1) so the scanned sheet stays bubbles-only.",
      version, n_pages, pdf_path), call. = FALSE)
  }
  invisible(n_pages)
}
