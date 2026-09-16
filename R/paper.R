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
