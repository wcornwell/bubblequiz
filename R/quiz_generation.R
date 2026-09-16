# quiz_generation.R -- turn lecture audio/transcripts into bubblequiz questions.

openai_api_key <- function(api_key = Sys.getenv("OPENAI_API_KEY")) {
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY is not set. Export it, or pass api_key =.", call. = FALSE)
  }
  api_key
}

is_url <- function(x) {
  length(x) == 1 && grepl("^https?://", x)
}

collapse_transcript_lines <- function(lines) {
  lines <- gsub("\r", "", lines, fixed = TRUE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^WEBVTT|^Kind:|^Language:", lines)]
  lines <- lines[!grepl("^[0-9]+$", lines)]
  lines <- lines[!grepl("-->", lines, fixed = TRUE)]
  lines <- gsub("<[^>]+>", "", lines)
  lines <- gsub("&amp;", "\\&", lines, fixed = FALSE)
  lines <- gsub("&lt;", "<", lines, fixed = TRUE)
  lines <- gsub("&gt;", ">", lines, fixed = TRUE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[c(TRUE, lines[-1] != lines[-length(lines)])]
  paste(lines, collapse = " ")
}

run_ytdlp <- function(args) {
  if (Sys.which("yt-dlp") == "") {
    stop("yt-dlp was not found on PATH. Install yt-dlp to read YouTube transcripts or audio.",
         call. = FALSE)
  }
  err <- tempfile("yt-dlp-stderr-")
  on.exit(unlink(err), add = TRUE)
  out <- system2("yt-dlp", args, stdout = TRUE, stderr = err)
  status <- attr(out, "status") %||% 0L
  if (!identical(status, 0L)) {
    detail <- if (file.exists(err)) readLines(err, warn = FALSE) else character(0)
    stop(paste(c(out, detail), collapse = "\n"), call. = FALSE)
  }
  out
}

youtube_caption_transcript <- function(url, lang = "en") {
  tmp <- tempfile("bubblequiz-youtube-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  out_tpl <- file.path(tmp, "lecture")

  args <- c(
    "--skip-download",
    "--write-subs",
    "--write-auto-subs",
    "--sub-langs", lang,
    "--sub-format", "vtt/srt/best",
    "-o", out_tpl,
    url
  )
  invisible(tryCatch(run_ytdlp(args), error = function(e) NULL))

  caption_files <- list.files(tmp, pattern = "[.](vtt|srt)$",
                              full.names = TRUE, ignore.case = TRUE)
  if (length(caption_files) == 0) return(NULL)
  text <- collapse_transcript_lines(readLines(caption_files[1], warn = FALSE))
  if (nzchar(trimws(text))) text else NULL
}

youtube_audio_file <- function(url) {
  tmp <- tempfile("bubblequiz-youtube-audio-")
  dir.create(tmp)
  out_tpl <- file.path(tmp, "lecture")
  args <- c(
    "-x",
    "--audio-format", "m4a",
    "-o", out_tpl,
    url
  )
  run_ytdlp(args)
  audio <- list.files(tmp, pattern = "lecture([.].*)?$", full.names = TRUE)
  audio <- audio[!grepl("[.](vtt|srt|json|part)$", audio, ignore.case = TRUE)]
  if (length(audio) == 0) {
    unlink(tmp, recursive = TRUE)
    stop("yt-dlp did not produce an m4a audio file.", call. = FALSE)
  }
  attr(audio[1], "cleanup_dir") <- tmp
  audio[1]
}

transcribe_media_file <- function(media,
                                  model = "gpt-4o-mini-transcribe",
                                  api_key = Sys.getenv("OPENAI_API_KEY"),
                                  prompt = NULL) {
  api_key <- openai_api_key(api_key)
  if (!file.exists(media)) stop("Media file not found: ", media, call. = FALSE)

  parts <- list(
    file = curl::form_file(media),
    model = model
  )
  if (!is.null(prompt) && nzchar(prompt)) parts$prompt <- prompt

  resp <- httr2::request("https://api.openai.com/v1/audio/transcriptions") |>
    httr2::req_headers("Authorization" = paste("Bearer", api_key)) |>
    httr2::req_body_multipart(!!!parts) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp)
  parsed$text %||% ""
}

#' Transcribe a lecture recording or YouTube URL
#'
#' Local audio/video files are sent to OpenAI's audio transcription endpoint.
#' YouTube URLs use `yt-dlp` to fetch captions first; if captions are missing,
#' `yt-dlp` downloads audio and the audio is transcribed.
#'
#' @param media Path to an audio/video file, or a YouTube URL.
#' @param output Transcript output path.
#' @param model OpenAI transcription model.
#' @param api_key OpenAI API key; defaults to `$OPENAI_API_KEY`.
#' @param prompt Optional hint about course/topic vocabulary.
#' @param youtube_lang Caption language preference for YouTube URLs.
#' @return Invisibly, the transcript text.
#' @export
transcribe_lecture <- function(media,
                               output = "output/transcript.txt",
                               model = "gpt-4o-mini-transcribe",
                               api_key = Sys.getenv("OPENAI_API_KEY"),
                               prompt = NULL,
                               youtube_lang = "en") {
  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)

  if (is_url(media)) {
    message("Reading YouTube captions with yt-dlp: ", media)
    text <- youtube_caption_transcript(media, youtube_lang)
    if (is.null(text)) {
      message("No captions found; downloading audio for transcription.")
      audio <- youtube_audio_file(media)
      cleanup_dir <- attr(audio, "cleanup_dir")
      on.exit(unlink(cleanup_dir, recursive = TRUE), add = TRUE)
      text <- transcribe_media_file(audio, model, api_key, prompt)
    }
  } else {
    text <- transcribe_media_file(media, model, api_key, prompt)
  }

  writeLines(text, output)
  message("Transcript written: ", output)
  invisible(text)
}

quiz_json_schema <- function(n_questions) {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = list("questions"),
    properties = list(
      questions = list(
        type = "array",
        minItems = n_questions,
        maxItems = n_questions,
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = list("number", "question", "options", "answer", "rationale"),
          properties = list(
            number = list(type = "integer"),
            question = list(type = "string"),
            options = list(
              type = "array",
              minItems = 5,
              maxItems = 5,
              items = list(type = "string")
            ),
            answer = list(type = "string", enum = list("A", "B", "C", "D", "E")),
            rationale = list(type = "string")
          )
        )
      )
    )
  )
}

quiz_generation_prompt <- function(cfg, transcript, n_questions) {
  marks <- unique(vapply(cfg$sections, function(s) as.character(s$marks_each %||% 1), character(1)))
  marks <- marks[nzchar(marks)]
  marks_label <- if (length(marks) == 1) paste0(marks, " mark") else "1 mark"

  paste(
    "Generate a multiple-choice quiz from the lecture transcript.",
    "",
    "Requirements:",
    sprintf("- Create exactly %d questions.", n_questions),
    "- Each question must test understanding, interpretation, or application, not trivia.",
    "- Each question has exactly five plausible options.",
    "- Use answer letters A, B, C, D, E.",
    "- Include a concise rationale that cites the lecture idea being tested.",
    "- Avoid questions that require information not present in the transcript.",
    sprintf("- These are for %s; write at university level.", cfg$course %||% "the course"),
    sprintf("- Default mark label for markdown headings: [%s].", marks_label),
    "",
    "Transcript:",
    transcript,
    sep = "\n"
  )
}

extract_response_text <- function(parsed) {
  if (!is.null(parsed$output_text)) return(as.character(parsed$output_text))
  pieces <- character(0)
  for (item in parsed$output %||% list()) {
    for (content in item$content %||% list()) {
      if (!is.null(content$text)) pieces <- c(pieces, content$text)
    }
  }
  paste(pieces, collapse = "\n")
}

questions_to_markdown <- function(quiz, marks_each = 1) {
  qs <- quiz$questions
  lines <- character(0)
  for (i in seq_along(qs)) {
    q <- qs[[i]]
    number <- q$number %||% i
    opts <- as.character(q$options)
    if (length(opts) != 5) stop("Generated question ", number, " does not have 5 options.", call. = FALSE)
    lines <- c(
      lines,
      sprintf("**Question %d [%s mark%s]:** %s",
              as.integer(number), marks_each, if (as.numeric(marks_each) == 1) "" else "s",
              trimws(as.character(q$question))),
      "",
      sprintf("%s. %s", letters[seq_along(opts)], trimws(opts)),
      "",
      sprintf("<!-- Answer %s -- %s -->",
              toupper(trimws(as.character(q$answer))),
              trimws(as.character(q$rationale %||% ""))),
      ""
    )
  }
  lines
}

#' Generate a quiz from a lecture transcript
#'
#' Writes a `questions.md` file in the format consumed by [generate_versions()].
#'
#' @param transcript Path to a transcript text file, or transcript text.
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param output Output markdown path.
#' @param model OpenAI model for quiz generation.
#' @param api_key OpenAI API key; defaults to `$OPENAI_API_KEY`.
#' @param n_questions Number of MCQs to generate; defaults to the config count.
#' @return Invisibly, the generated quiz object.
#' @export
generate_quiz_from_transcript <- function(transcript,
                                          config = default_config_path(),
                                          output = "questions.md",
                                          model = "gpt-5",
                                          api_key = Sys.getenv("OPENAI_API_KEY"),
                                          n_questions = NULL) {
  api_key <- openai_api_key(api_key)
  cfg <- if (is.list(config)) config else load_exam_config(config)
  if (length(transcript) == 1 && file.exists(transcript)) {
    transcript_text <- paste(readLines(transcript, warn = FALSE), collapse = "\n")
  } else {
    transcript_text <- paste(transcript, collapse = "\n")
  }
  if (!nzchar(trimws(transcript_text))) stop("Transcript is empty.", call. = FALSE)

  n_questions <- n_questions %||% length(cfg$questions)
  if (is.na(n_questions) || n_questions < 1) stop("n_questions must be >= 1.", call. = FALSE)

  prompt <- quiz_generation_prompt(cfg, transcript_text, n_questions)
  body <- list(
    model = model,
    input = list(
      list(
        role = "system",
        content = "You write assessment-quality multiple-choice questions for university teaching."
      ),
      list(role = "user", content = prompt)
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "lecture_quiz",
        strict = TRUE,
        schema = quiz_json_schema(n_questions)
      )
    )
  )

  resp <- httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  raw_text <- extract_response_text(parsed)
  quiz <- tryCatch(jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(quiz) || is.null(quiz$questions)) {
    stop("OpenAI response did not contain valid quiz JSON.", call. = FALSE)
  }

  first_marks <- cfg$sections[[1]]$marks_each %||% 1
  lines <- questions_to_markdown(quiz, first_marks)
  writeLines(lines, output)
  message("Quiz written: ", output)
  invisible(quiz)
}
