# bubblequiz

`bubblequiz` builds small paper multiple-choice quizzes from a course repository, prints randomized QR-coded forms, scans completed sheets, and marks them with deterministic computer vision.

The package is the reusable engine. Course-specific files such as `exam.yml`, `questions.md`, transcripts, scans, and outputs live in an external course folder.

## What It Does

- Defines quiz shape once in `exam.yml`.
- Reads YouTube captions with `yt-dlp` before falling back to audio transcription.
- Overgenerates candidate questions from a lecture transcript for instructor selection.
- Checks whether drafted questions are supported by the lecture transcript.
- Generates multiple randomized versions from `questions.md`.
- Prints one integrated form per version: questions, answer bubbles, zID bubbles, and QR metadata.
- Combines all versions into one print PDF for easy duplex printing.
- Reads completed scans with a local computer-vision marker.
- Scores each page against the correct version-specific answer key.

## Worked Example

A complete generated example is included in the general tool repository at:

```text
example/six-question-quiz/
```

This is intentionally laid out like a small external course repository: it has
its own `exam.yml`, `questions.md`, generated version files, answer key, and
print PDF. The main file to inspect or print is:

```text
example/six-question-quiz/output/quizforms_all_versions.pdf
```

For the example, each student receives one double-sided sheet. The combined PDF is ordered by version:

```text
Version 1: pages 1-2
Version 2: pages 3-4
Version 3: pages 5-6
Version 4: pages 7-8
```

The manifest explains exactly what is in the example and how to rebuild it:

```text
example/six-question-quiz/MANIFEST.md
```

The worked example is primarily a printing and version-tracking example. With
the current inline form renderer, local CV marking is calibrated per physical
page, so production quizzes should be kept to one page per version until
multi-page marking is added.

## Course Folder Layout

A course folder usually looks like this:

```text
my-quiz/
  exam.yml
  questions.md
  output/
  scans.pdf
```

`exam.yml` controls the structure:

```yaml
course: BES2041
title: "BES2041 Lecture Quiz"
date: "16 September 2026"
subtitle: "Multiple Choice Quiz"
duration: "10 min"
total_marks: 6
versions: 4
options: [A, B, C, D, E]

id:
  label: zID
  prefix: z
  digits: 7

layout:
  columns: 1

sections:
  - id: A
    title: "Quantitative skills and coding"
    questions: [1, 2, 3, 4, 5, 6]
    marks_each: 1
```

`questions.md` contains questions, options, and answer comments:

```markdown
**Question 1 [1 mark]:** What is the best interpretation of RMSE?

a. A measure of absolute prediction error in response units
b. A test of whether the intercept is zero
c. A measure that always increases with sample size
d. A correlation coefficient
e. A p-value

<!-- Answer A -- RMSE is expressed in the response variable's units. -->
```

## Draft Questions From A Lecture

For YouTube videos, install `yt-dlp` and let `bubblequiz` try captions first:

```r
transcribe_lecture(
  "https://www.youtube.com/watch?v=Xrw0G-Pt1fI",
  output = "output/transcript.txt"
)
```

If captions are available, this does not use the audio transcription API. If no
captions are available, it downloads audio and transcribes it.

To overgenerate questions for instructor review:

```r
generate_question_candidates(
  transcript = "output/transcript.txt",
  config = "exam.yml",
  output = "output/question_candidates.md",
  n_candidates = 14
)
```

Read `output/question_candidates.md`, choose the strongest candidate question
numbers, then create the final quiz source:

```r
select_questions(
  candidates = "output/question_candidates.md",
  selected = c(2, 4, 5, 8, 11, 13),
  output = "questions.md"
)
```

To check a hand-written or selected quiz against the lecture transcript:

```r
check_questions_in_transcript(
  questions = "questions.md",
  transcript = "output/transcript.txt",
  output = "output/question_coverage.csv"
)
```

`question_coverage.csv` flags questions whose question text, answer, or core
concept is not clearly supported by the lecture.

The same workflow is available from the command line:

```bash
bubblequiz transcribe "https://www.youtube.com/watch?v=Xrw0G-Pt1fI"
bubblequiz candidates --transcript output/transcript.txt --n-candidates 14
bubblequiz select --selected 2,4,5,8,11,13
bubblequiz verify --transcript output/transcript.txt
```

## Build A Quiz

From a course folder:

```r
for (f in list.files("/path/to/bubblequiz/R", "[.]R$", full.names = TRUE)) source(f)

generate_versions("exam.yml", "questions.md", "output")
make_quiz_forms("exam.yml", "output")
combine_quiz_forms("exam.yml", "output")
calibrate_coords("exam.yml", "output/quizform_v1.pdf", "output/layout.R", "output/layout_preview.jpeg")
```

Print:

```text
output/quizforms_all_versions.pdf
```

Use duplex printing. For six questions, each version fits on one double-sided sheet.

## Scan Settings

Use:

```text
Color or grayscale
300 dpi
No auto-crop if possible
No text enhancement / high-contrast cleanup
No auto-rotate if possible
PDF output
```

Students should use black or dark blue pen and fill bubbles completely.

## Mark And Score

Place the scan PDF in the course folder, for example:

```text
scans.pdf
```

Then run:

```r
preprocess_scans("scans.pdf", "exam.yml", force = TRUE)
mark_scans_cv("scans", config = "exam.yml", layout = "output/layout.R")
score_results("scans", key = "output/answer_key.csv", config = "exam.yml")
```

Outputs:

```text
scans/progress.csv
scans/marked-cv/
scans/results.csv
```

Rows needing manual review are flagged in `progress.csv`.

## Version Tracking

Each printed form includes:

- visible version text, e.g. `Version 3`
- a QR code containing machine-readable metadata

Example QR payload:

```text
bubblequiz|course=BES2041|version=3|questions=1,2,3,4,5,6
```

The CV marker uses this QR code to select the correct answer-key column, such as `answer_v3`.

## zID Reading

The form includes a zID digit grid. Students fill one digit per column. The CV marker reads those bubbles directly from the scan.

If a digit is ambiguous, the row is flagged for manual review.

## Question Count

With the current layout and moderately wordy questions:

```text
6-8 questions: comfortable
10 questions: practical upper limit for one double-sided sheet
11+ questions: likely spills beyond one double-sided sheet
```

Shorter questions fit better; longer questions may reduce the limit.

For the current local CV marker, prefer a quiz that fits on one printed page
per version. Multi-page/duplex print forms are useful for review and classroom
handling, but the marker still needs page-aware aggregation before they should
be used for live marking.

## External Tools

For QR decoding:

```bash
brew install zbar
```

For rendering PDFs:

- XeLaTeX
- TeX Live package `qrcode`

The current macOS test environment has both available.
