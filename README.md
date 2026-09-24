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
Grayscale (colour works, but files are ~3x larger and it reads no better)
200-300 dpi
No auto-crop if possible
No text enhancement / high-contrast cleanup
No auto-rotate if possible
PDF output
```

Students should use black or dark blue pen and fill bubbles completely.

## Check A Large Stack Before Marking

A multi-page form is printed duplex, so a 150-student quiz arrives as one
~300-page PDF whose pages must read 1,2,1,2 and so on. Sheet feeders swallow
pages, double-feed and occasionally reverse a sheet, and none of that is
visible in the page images alone. Every page carries a QR giving its version,
its page number and the sheet length, so the stack can be checked before any
marking happens:

```r
preprocess_scans("scans.pdf", "exam.yml", force = TRUE)
check_scan_sequence("scans")
```

Output:

```text
Pages:  300
Sheets: 150 complete
Sequence is clean; every page belongs to a complete sheet.
```

or, when the feeder misbehaved:

```text
PROBLEM PAGES: 1 -- rescan or handle these before marking
  page 7    page_0007.png          orphan page 2 (no page 1 before it)
```

`scans/scan_sequence.csv` has a row per page with its assigned `sheet` number.
One feeder error does not cascade: the walk resynchronises at the next page 1,
so the rest of the stack still groups correctly and only the affected sheet
needs rescanning.

## Mark And Score

For each quiz, put every scan PDF in one folder and run:

```r
mark_quiz("week2_scans", config = "exam.yml", forms = "output",
          grade_item = "Week 2 quiz")
```

That renders, checks, marks and scores every scan in the folder, and writes:

```text
week2_scans/moodle_import.csv   Username + grade: clean and resolved sheets only
week2_scans/review.csv          every sheet that needs a person, and your decisions
```

Work through `review.csv`, run `mark_quiz()` again (scans already marked are
not marked again, so it takes seconds), and upload `moodle_import.csv` once
nothing is left open. In Moodle: Grades > Import > CSV file; map `Username` to
"username" and the grade column to the grade item. Rendering needs `pdftoppm`
(poppler) and QR reading needs `zbarimg` (zbar) on the PATH.

The steps `mark_quiz()` runs, for use one at a time:

```r
preprocess_scans("scans.pdf", "exam.yml", force = TRUE)
check_scan_sequence("scans")
mark_scans_cv("scans", config = "exam.yml", layout = "output/layout.R")
aggregate_sheets("scans", config = "exam.yml")
score_results("scans", key = "output/answer_key.csv", config = "exam.yml",
              review = "review.csv")
export_moodle("scans", output = "moodle_import.csv", review = "review.csv",
              grade_item = "Week 2 quiz", config = "exam.yml")
```

Per scan folder:

```text
scans/scan_sequence.csv   one row per page: version, page number, sheet
scans/progress.csv        one row per page, as read by the marker
scans/marked-cv/          each page with the recorded answers drawn on
scans/sheets.csv          one row per student, pages joined
scans/results.csv         scores, with needs_review, notes and answers as read
```

### Reviewing Flagged Sheets

The marker never guesses. A sheet is flagged when anything on it was not read
cleanly, and `review.csv` says why:

```text
two marks          two bubbles filled in one row -- usually a correction;
                   the crossed-out one is often the darker, so it is not chosen
faint mark         a mark too light to accept
blank              an unanswered question, or an empty zID column
ambiguous          two bubbles too close to call
QR unreadable      version taken from the other side of the sheet
back side first    a sheet put through the scanner the wrong way over
```

Each row shows what was read -- `zid`, and `answers` as one letter per
question (`*` uncertain, `-` unanswered) -- and has four columns for you:

```text
correct_zid       the right zID, if the one read is wrong or has a ?
correct_answers   only the answers that change: Q5=A, or Q2=B; Q5=- (- = blank)
resolved          yes, once checked -- needed only when nothing changes
comment           free text, kept as written
```

A sheet goes into the upload once decided, but never while it still holds an
uncertain answer (`B*`) or an invalid zID: those must be set explicitly. Rows
are never dropped and your columns -- including any you add -- are never
overwritten, so the file is the record of what was decided. A typo in a
decision stops the run and names the row.

Corrections can also go in `overrides.csv` in a scan folder
(`file,page,zid,name,question,response`, with `question` a number, `zid` or
`ok`); `preprocess_scans()` never overwrites it.

For a single-page form, `check_scan_sequence()` and `aggregate_sheets()` are
harmless no-ops: every page is its own sheet and `sheets.csv` matches
`progress.csv`. For a multi-page form they are required, because the marker
reads one page at a time and only the front of a sheet carries a zID grid.

### How A Multi-Page Form Is Marked

```text
calibrate_coords()     reads every page of the blank form and records which
                       page each question's bubble row is printed on
check_scan_sequence()  reads the per-page QR and groups the stack into sheets
mark_scans_cv()        reads only the questions printed on each page, and the
                       zID from the front page only
aggregate_sheets()     joins the pages of each sheet into one student record
score_results()        scores sheets.csv when present, otherwise progress.csv
```

A page that cannot be attributed to a complete sheet is excluded from
`sheets.csv` with a warning rather than being scored as a whole paper, and a
sheet that is short a page is flagged for review.

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
