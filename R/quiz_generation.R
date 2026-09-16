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

quiz_generation_prompt <- function(cfg, transcript, n_questions, candidate_mode = FALSE) {
  marks <- unique(vapply(cfg$sections, function(s) as.character(s$marks_each %||% 1), character(1)))
  marks <- marks[nzchar(marks)]
  marks_label <- if (length(marks) == 1) paste0(marks, " mark") else "1 mark"
  mode_line <- if (isTRUE(candidate_mode)) {
    "- This is a candidate bank for instructor review; prefer variety and do not repeat the same lecture point."
  } else {
    "- The output will be used directly as the quiz source."
  }

  paste(
    "Generate a multiple-choice quiz from the lecture transcript.",
    "",
    "Requirements:",
    sprintf("- Create exactly %d questions.", n_questions),
    mode_line,
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

question_heading_regex <- "^\\*\\*Question[[:space:]]+([0-9]+)[^:]*:\\*\\*"

split_question_markdown <- function(lines) {
  starts <- grep(question_heading_regex, lines)
  if (length(starts) == 0) {
    stop("No question headings found. Expected lines like '**Question 1 [1 mark]:** ...'",
         call. = FALSE)
  }
  ends <- c(starts[-1] - 1L, length(lines))
  prefix <- if (starts[1] > 1L) lines[seq_len(starts[1] - 1L)] else character(0)
  blocks <- lapply(seq_along(starts), function(i) lines[starts[i]:ends[i]])
  numbers <- vapply(blocks, function(block) {
    as.integer(sub(paste0(question_heading_regex, ".*$"), "\\1", block[1]))
  }, integer(1))
  list(prefix = prefix, numbers = numbers, blocks = blocks)
}

renumber_question_block <- function(block, number) {
  block[1] <- sub("^\\*\\*Question[[:space:]]+[0-9]+", paste0("**Question ", number), block[1])
  block
}

normalise_selected_questions <- function(selected) {
  if (length(selected) == 1L && grepl(",", selected, fixed = TRUE)) {
    selected <- strsplit(selected, ",", fixed = TRUE)[[1]]
  }
  selected <- trimws(as.character(selected))
  selected <- selected[nzchar(selected)]
  nums <- suppressWarnings(as.integer(selected))
  if (anyNA(nums) || length(nums) == 0) stop("selected must contain question numbers.", call. = FALSE)
  nums
}

read_transcript_text <- function(transcript) {
  if (length(transcript) == 1 && is_url(transcript)) {
    tmp <- tempfile("bubblequiz-transcript-", fileext = ".txt")
    return(transcribe_lecture(transcript, output = tmp))
  }
  media_ext <- c("aac", "aiff", "flac", "m4a", "mkv", "mov", "mp3", "mp4",
                 "mpeg", "mpga", "oga", "ogg", "wav", "webm")
  if (length(transcript) == 1 && file.exists(transcript) &&
      tolower(tools::file_ext(transcript)) %in% media_ext) {
    tmp <- tempfile("bubblequiz-transcript-", fileext = ".txt")
    return(transcribe_lecture(transcript, output = tmp))
  }
  if (length(transcript) == 1 && file.exists(transcript)) {
    paste(readLines(transcript, warn = FALSE), collapse = "\n")
  } else {
    paste(transcript, collapse = "\n")
  }
}

call_quiz_generation_api <- function(cfg, transcript_text, n_questions, model, api_key,
                                     candidate_mode = FALSE) {
  prompt <- quiz_generation_prompt(cfg, transcript_text, n_questions, candidate_mode)
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
  quiz
}

#' Generate a quiz from a lecture transcript
#'
#' Writes a `questions.md` file in the format consumed by [generate_versions()].
#'
#' @param transcript Path to a transcript text file, transcript text, a YouTube
#'   URL, or a local audio/video recording.
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
  transcript_text <- read_transcript_text(transcript)
  if (!nzchar(trimws(transcript_text))) stop("Transcript is empty.", call. = FALSE)

  n_questions <- n_questions %||% length(cfg$questions)
  if (is.na(n_questions) || n_questions < 1) stop("n_questions must be >= 1.", call. = FALSE)

  quiz <- call_quiz_generation_api(cfg, transcript_text, n_questions, model, api_key)
  first_marks <- cfg$sections[[1]]$marks_each %||% 1
  lines <- questions_to_markdown(quiz, first_marks)
  writeLines(lines, output)
  message("Quiz written: ", output)
  invisible(quiz)
}

#' Generate an oversized question candidate bank
#'
#' Writes more questions than the final quiz needs, so the instructor can choose
#' the strongest ones before calling [select_questions()].
#'
#' @param transcript Path to a transcript text file, transcript text, a YouTube
#'   URL, or a local audio/video recording.
#' @param config Path to the exam config YAML, or a loaded config list.
#' @param output Output markdown path for the candidate bank.
#' @param model OpenAI model for quiz generation.
#' @param api_key OpenAI API key; defaults to `$OPENAI_API_KEY`.
#' @param n_candidates Number of candidate MCQs to generate.
#' @return Invisibly, the generated quiz object.
#' @export
generate_question_candidates <- function(transcript,
                                         config = default_config_path(),
                                         output = "output/question_candidates.md",
                                         model = "gpt-5",
                                         api_key = Sys.getenv("OPENAI_API_KEY"),
                                         n_candidates = NULL) {
  api_key <- openai_api_key(api_key)
  cfg <- if (is.list(config)) config else load_exam_config(config)
  transcript_text <- read_transcript_text(transcript)
  if (!nzchar(trimws(transcript_text))) stop("Transcript is empty.", call. = FALSE)

  target <- length(cfg$questions)
  n_candidates <- n_candidates %||% max(target + 4L, target * 2L)
  if (is.na(n_candidates) || n_candidates < target) {
    stop("n_candidates must be at least the configured quiz question count.", call. = FALSE)
  }

  quiz <- call_quiz_generation_api(cfg, transcript_text, n_candidates, model, api_key,
                                   candidate_mode = TRUE)
  first_marks <- cfg$sections[[1]]$marks_each %||% 1
  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)
  writeLines(questions_to_markdown(quiz, first_marks), output)
  message("Candidate questions written: ", output)
  invisible(quiz)
}

#' Select final questions from a candidate bank
#'
#' @param candidates Markdown file containing generated candidate questions.
#' @param selected Candidate question numbers to keep, in final quiz order.
#' @param output Output `questions.md` path.
#' @param renumber Whether to renumber selected questions as 1..N.
#' @return Invisibly, the selected question numbers.
#' @export
select_questions <- function(candidates = "output/question_candidates.md",
                             selected,
                             output = "questions.md",
                             renumber = TRUE) {
  if (!file.exists(candidates)) stop("Candidate file not found: ", candidates, call. = FALSE)
  keep <- normalise_selected_questions(selected)
  parsed <- split_question_markdown(readLines(candidates, warn = FALSE))
  missing <- setdiff(keep, parsed$numbers)
  if (length(missing)) {
    stop("Selected question(s) not found in candidate bank: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  lines <- character(0)
  for (i in seq_along(keep)) {
    idx <- match(keep[i], parsed$numbers)
    block <- parsed$blocks[[idx]]
    if (isTRUE(renumber)) block <- renumber_question_block(block, i)
    lines <- c(lines, block, "")
  }
  writeLines(lines, output)
  message("Selected questions written: ", output)
  invisible(keep)
}

question_coverage_schema <- function(n_questions) {
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
          required = list("number", "supported", "confidence", "evidence", "concern"),
          properties = list(
            number = list(type = "integer"),
            supported = list(type = "boolean"),
            confidence = list(type = "string", enum = list("high", "medium", "low")),
            evidence = list(type = "string"),
            concern = list(type = "string")
          )
        )
      )
    )
  )
}

#' Check whether quiz questions are grounded in a lecture transcript
#'
#' Produces a CSV report showing whether each question appears supported by the
#' lecture, with short evidence and concerns for instructor review.
#'
#' @param questions Markdown question file.
#' @param transcript Path to a transcript text file, transcript text, a YouTube
#'   URL, or a local audio/video recording.
#' @param output Output CSV path.
#' @param model OpenAI model for the coverage check.
#' @param api_key OpenAI API key; defaults to `$OPENAI_API_KEY`.
#' @return Invisibly, a data frame with the coverage report.
#' @export
check_questions_in_transcript <- function(questions = "questions.md",
                                          transcript,
                                          output = "output/question_coverage.csv",
                                          model = "gpt-5",
                                          api_key = Sys.getenv("OPENAI_API_KEY")) {
  api_key <- openai_api_key(api_key)
  if (!file.exists(questions)) stop("Question file not found: ", questions, call. = FALSE)
  question_text <- paste(readLines(questions, warn = FALSE), collapse = "\n")
  parsed_questions <- split_question_markdown(readLines(questions, warn = FALSE))
  transcript_text <- read_transcript_text(transcript)
  if (!nzchar(trimws(transcript_text))) stop("Transcript is empty.", call. = FALSE)

  prompt <- paste(
    "Check whether each quiz question is grounded in the lecture transcript.",
    "Mark supported=false if the question, correct answer, or core concept cannot be justified from the transcript.",
    "Use short paraphrased evidence; do not invent quotes.",
    "",
    "Questions:",
    question_text,
    "",
    "Transcript:",
    transcript_text,
    sep = "\n"
  )

  body <- list(
    model = model,
    input = list(
      list(role = "system",
           content = "You audit assessment questions against lecture transcripts for factual grounding."),
      list(role = "user", content = prompt)
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "question_coverage",
        strict = TRUE,
        schema = question_coverage_schema(length(parsed_questions$numbers))
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

  raw_text <- extract_response_text(httr2::resp_body_json(resp, simplifyVector = FALSE))
  report <- tryCatch(jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(report) || is.null(report$questions)) {
    stop("OpenAI response did not contain valid coverage JSON.", call. = FALSE)
  }

  df <- do.call(rbind, lapply(report$questions, function(x) {
    data.frame(
      question = as.integer(x$number),
      supported = isTRUE(x$supported),
      confidence = as.character(x$confidence),
      evidence = as.character(x$evidence),
      concern = as.character(x$concern),
      stringsAsFactors = FALSE
    )
  }))
  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(df, output, row.names = FALSE)
  message("Coverage report written: ", output)
  invisible(df)
}
