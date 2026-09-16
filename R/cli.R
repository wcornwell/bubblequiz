# cli.R -- the `bubblequiz` command-line front end.
#
# Course repositories drive the pipeline from a Makefile or a shell, not from
# an R session, so every step is reachable as a subcommand. All project paths
# (exam.yml, questions.md, output/, scans/) resolve against the current working
# directory -- the course repo -- never against the package.

bq_usage <- function() {
  cat("
bubblequiz -- print, scan and auto-mark multiple-choice bubble sheets

Usage: bubblequiz <command> [options]

Commands:
  init [--dir .]            Scaffold exam.yml, questions.md and a Makefile
                            into a course repository
  transcribe <media-or-url>  lecture recording/YouTube -> transcript text
  quiz --transcript <file>   transcript text -> questions.md
  versions                  questions.md -> output/questions_v*.md + answer_key.csv
  sheets                    exam.yml     -> output/bubblesheet_v*.pdf
  calibrate                 rendered sheet -> output/layout.R + preview JPEG
  paper                     output/questions_v*.md -> printable PDFs
  build                     versions + sheets + calibrate + paper
  preprocess <scan.pdf>     scanned PDF  -> page images + progress.csv
  mark --dir <folder>       grade scanned pages with the Claude vision API
  score --dir <folder>      progress.csv + answer_key.csv -> results.csv
  check                     validate exam.yml and report the derived layout

Common options:
  --config <path>     Exam config YAML          [default: exam.yml]
  --outdir <path>     Output directory          [default: output]
  --questions <path>  Question source markdown  [default: questions.md]
  --transcript <path> Lecture transcript text
  --n-questions <n>   Number of generated MCQs   [default: config MCQ count]
  --layout <path>     Calibrated layout file    [default: output/layout.R]
  --key <path>        Answer key CSV            [default: output/answer_key.csv]
  --model <id>        Anthropic model           [default: claude-sonnet-4-6]
  --openai-model <id> OpenAI quiz model          [default: gpt-5]
  --transcribe-model  OpenAI transcription model [default: gpt-4o-mini-transcribe]
  --youtube-lang <id> YouTube caption language   [default: en]
  --dpi <n>           Scan rasterisation DPI    [default: 150]
  --dry-run           Mark only the first 3 pending pages
  --force             Re-extract scans over an existing folder

Marking needs ANTHROPIC_API_KEY in the environment.
")
}

# Minimal flag parser: --flag value, or --flag for switches.
bq_parse_args <- function(args) {
  opts <- list()
  pos  <- character(0)
  i <- 1L
  switches <- c("dry-run", "force", "no-render", "help")
  while (i <= length(args)) {
    a <- args[i]
    if (grepl("^--", a)) {
      name <- sub("^--", "", a)
      if (grepl("=", name, fixed = TRUE)) {
        parts <- strsplit(name, "=", fixed = TRUE)[[1]]
        opts[[parts[1]]] <- paste(parts[-1], collapse = "=")
      } else if (name %in% switches) {
        opts[[name]] <- TRUE
      } else {
        if (i == length(args)) stop("Missing value for --", name, call. = FALSE)
        opts[[name]] <- args[i + 1L]
        i <- i + 1L
      }
    } else {
      pos <- c(pos, a)
    }
    i <- i + 1L
  }
  list(opts = opts, pos = pos)
}

#' Run the bubblequiz command-line interface
#'
#' @param args Command-line arguments, defaulting to those of the current session.
#' @return Invisibly, the result of the dispatched command.
#' @export
bq_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) == 0 || args[1] %in% c("-h", "--help", "help")) {
    bq_usage()
    return(invisible(NULL))
  }
  cmd    <- args[1]
  parsed <- bq_parse_args(args[-1])
  o      <- parsed$opts
  pos    <- parsed$pos

  config    <- o$config    %||% default_config_path()
  outdir    <- o$outdir    %||% "output"
  questions <- o$questions %||% "questions.md"
  layout    <- o$layout    %||% file.path(outdir, "layout.R")
  key       <- o$key       %||% file.path(outdir, "answer_key.csv")

  res <- switch(cmd,
    "init" = init_course(o$dir %||% "."),

    "transcribe" = {
      if (length(pos) == 0) stop("Usage: bubblequiz transcribe <audio/video>", call. = FALSE)
      transcribe_lecture(
        media       = pos[1],
        output      = o$output %||% file.path(outdir, "transcript.txt"),
        model       = o[["transcribe-model"]] %||% "gpt-4o-mini-transcribe",
        prompt      = o$prompt %||% NULL,
        youtube_lang = o[["youtube-lang"]] %||% "en")
    },

    "quiz" = {
      transcript <- o$transcript %||% if (length(pos) > 0) pos[1] else NULL
      if (is.null(transcript)) stop("Usage: bubblequiz quiz --transcript <file>", call. = FALSE)
      generate_quiz_from_transcript(
        transcript = transcript,
        config     = config,
        output     = questions,
        model      = o[["openai-model"]] %||% "gpt-5",
        n_questions = if (is.null(o[["n-questions"]])) NULL else as.integer(o[["n-questions"]]))
    },

    "versions" = generate_versions(config, questions, outdir),

    "sheets" = make_bubblesheet(config, outdir, render = !isTRUE(o[["no-render"]])),

    "calibrate" = calibrate_coords(
      config,
      pdf     = o$pdf %||% file.path(outdir, "bubblesheet_v1.pdf"),
      out     = layout,
      preview = file.path(outdir, "layout_preview.jpeg")),

    "paper" = render_papers(config, outdir),

    "build" = {
      generate_versions(config, questions, outdir)
      make_bubblesheet(config, outdir)
      calibrate_coords(config, file.path(outdir, "bubblesheet_v1.pdf"), layout,
                       file.path(outdir, "layout_preview.jpeg"))
      render_papers(config, outdir)
    },

    "preprocess" = {
      if (length(pos) == 0) stop("Usage: bubblequiz preprocess <scan.pdf>", call. = FALSE)
      preprocess_scans(pos[1], config,
                       dpi   = as.integer(o$dpi %||% 150),
                       force = isTRUE(o$force))
    },

    "mark" = {
      if (is.null(o$dir)) stop("Usage: bubblequiz mark --dir <folder>", call. = FALSE)
      mark_scans(o$dir, config, layout,
                 model   = o$model %||% "claude-sonnet-4-6",
                 dry_run = isTRUE(o[["dry-run"]]))
    },

    "score" = {
      if (is.null(o$dir)) stop("Usage: bubblequiz score --dir <folder>", call. = FALSE)
      score_results(o$dir, key, config, o$output)
    },

    "check" = check_config(config),

    {
      bq_usage()
      stop("Unknown command: ", cmd, call. = FALSE)
    }
  )
  invisible(res)
}

#' Validate an exam config and print the layout it implies
#'
#' The fastest way to see what a config change actually did before committing
#' a printed sheet to it.
#'
#' @param config Path to the exam config YAML, or a loaded config list.
#' @return Invisibly, the loaded config.
#' @export
check_config <- function(config = default_config_path()) {
  cfg <- if (is.list(config)) config else load_exam_config(config)

  cat(sprintf("Config     : %s\n", cfg$path))
  cat(sprintf("Course     : %s\n", cfg$course))
  cat(sprintf("Title      : %s\n", cfg$title))
  cat(sprintf("Versions   : %s\n", paste(cfg$valid_versions, collapse = ", ")))
  cat(sprintf("Options    : %s\n", paste(cfg$options, collapse = " ")))
  cat(sprintf("MCQ (%d)   : %s\n", length(cfg$questions), paste(cfg$questions, collapse = " ")))
  cat(sprintf("Essays (%d): %s\n", length(cfg$essay_questions),
              if (length(cfg$essay_questions)) paste(cfg$essay_questions, collapse = " ") else "none"))
  cat(sprintf("ID         : %s, %d digits, prefix '%s'\n",
              cfg$id$label, cfg$id$digits, cfg$id$prefix))
  cat(sprintf("\nSheet layout (%d columns, %d bubble rows):\n",
              cfg$n_cols, length(cfg$bubble_rows)))

  for (sec in cfg$sections) {
    items <- c(sec$questions, vapply(sec$essays, `[[`, integer(1), "number"))
    if (length(items) == 0) next
    cat(sprintf("\n  Section %s -- %s\n", sec$id, sec$title))
    for (r in layout_section(items, cfg$n_cols)) {
      cells <- vapply(cfg$col_names, function(col) {
        q <- r[[col]]
        if (is.null(q)) return("          ")
        if (q %in% cfg$questions) sprintf("Q%-3d %s", q, paste(cfg$options, collapse = " "))
        else sprintf("Q%-3d [essay]", q)
      }, character(1))
      cat("    ", paste(cells, collapse = "   "), "\n", sep = "")
    }
  }
  cat("\nConfig is valid.\n")
  invisible(cfg)
}
