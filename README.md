# bubblequiz

[![R-CMD-check](https://github.com/wcornwell/bubblequiz/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/wcornwell/bubblequiz/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1-276DC3?logo=r&logoColor=white)

You need to give a low-stakes multiple-choice quiz, and you can no longer trust the obvious way to give it.

Put it on the learning platform and a student has an LLM open in the next tab, or
a browser extension that answers the question before they've finished reading it.
Locking down the browser, the room, and the network is an arms race you can't win
one quiz at a time. The old answer to this problem — a printed sheet, a pencil,
and a bubble to fill in — never had that problem, because there was nothing to
paste the question into. Most universities, though, dismantled the scantron
infrastructure that used to make paper multiple choice practical at scale: no
scanner, no proprietary form, no one left who runs the machine.

`bubblequiz` is a modern, personal-scale replacement for that machine. It
designs a quiz, typesets it to LaTeX with randomized question/option order and
QR-coded anchors, and — because it knows the exact geometry of the printed page
and where every bubble sits relative to those anchors — reads the scanned
answer sheets back with ordinary computer vision, entirely on your own machine.
A large language model could technically grade a bubble sheet too, but sending
a classroom's identifying ID numbers and answers to a cloud API is both a
privacy exposure you don't need and considerable overkill for a task that comes
down to "is this box dark or light." Where a mark is genuinely ambiguous —
a half-filled bubble, a smudge, a stray mark — bubblequiz doesn't guess; it
flags the row for a human to look at instead.

The package is the reusable engine. Course-specific files such as `exam.yml`, `questions.md`, transcripts, scans, and outputs live in an external course folder.

## What It Does

- Defines quiz shape once in `exam.yml`.
- Generates multiple randomized versions from `questions.md`, so students sitting next to each other don't have matching sheets to glance across.
- Prints one integrated form per version: questions, answer bubbles, student-ID bubbles, and a QR anchor carrying the version and page metadata.
- Combines all versions into one print PDF for easy duplex printing.
- Reads completed scans with a local computer-vision marker — no cloud call, no student data leaving your machine.
- Scores each student's sheet against the correct version-specific answer key, and flags anything ambiguous for manual review instead of silently guessing.

## Installation

```r
# install.packages("remotes")
remotes::install_github("wcornwell/bubblequiz")
```

bubblequiz requires R 4.1 or later. Printing and QR decoding also need some
external tools; see [External Tools](#external-tools).

## Worked Example

A complete generated example is included in this repository at:

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

`calibrate_coords()` reads every page of the rendered form, not just the
first, and `check_scan_sequence()` / `aggregate_sheets()` (below) group a
multi-page scan stack into one record per student before scoring. A
single-page quiz needs none of that machinery — every page is already a
whole sheet — so it stays the simplest case, but multi-page duplex forms are
marked live, not just printed for review.

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

### Figures and math

A question can show one figure, on its own line between the stem and the options:

```markdown
![Fig. 2 from Smith et al. (2021). Reproduced with permission.](figures/fig2.png){width=0.7}
```

The path is relative to `questions.md`; `width` is a fraction of the text width
(default 0.8); the caption is printed under the figure. Figures are capped in
height by `layout.figure_max_height` in `exam.yml` (default `7cm`). Figures take
up space on the answer sheet, which is capped at two pages, so keep them small
when a quiz is already long.

`$...$` in stems, options and captions is typeset as math (`$R^2$`, `$p < 0.05$`).
Dollar signs that do not pair up as math (`costs $5 and $10`) stay literal, and
`\$` is always a literal dollar sign. Everything else is escaped as before.

## Build A Quiz

Write `questions.md` by hand (or with whatever drafting tool you like — bubblequiz
doesn't care how the questions were written, only that they follow the format
above). From a course folder, with bubblequiz installed:

```r
library(bubblequiz)

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

## How The Geometry Pipeline Works

bubblequiz never looks at a scanned page and guesses where the bubbles are. It
renders the form, measures its own rendering, and reads scans back against
those exact measurements — the same trick a scantron machine did with a
physical template, done here with pixels.

**1. Every printed sheet carries four anchors.** A 5mm black square sits 3mm
inside each corner of the page, plus a QR code carrying the course, version,
and page number.

<img src="man/figures/geometry-registration-mark.png" width="260" alt="One corner of a printed sheet, showing the solid black registration square inset from the edge">

**2. `calibrate_coords()` reads the rendered PDF, not the LaTeX source.** It
pulls the (x, y) position of every printed option letter straight out of the
PDF's own text layer, clusters them into bubble rows and columns, and writes
the result to `layout.R` — one set of coordinates, normalised 0–1 across the
page, reused for every version and every scan of this quiz. A preview image
confirms the fit by stamping each detected coordinate back onto the blank
form:

<img src="man/figures/geometry-calibration.png" width="260" alt="Close-up of a blank answer row with A B C D E stamped in red exactly on top of the printed bubbles">

**3. A scan is just the same page, photographed.** Reading one back is a
three-step lookup, not a model: find the four registration squares in the
scanned image, fit a map from the calibrated page coordinates to this
particular photo's pixel coordinates (correcting for the page having shifted,
rotated, or scaled slightly in the scanner), then sample the ink darkness in
a small circle at every bubble center that mapping predicts.

<table>
<tr>
<td><img src="man/figures/geometry-bubble-blank.png" width="220" alt="Blank printed bubble row, options A through E"><br><sub>printed</sub></td>
<td><img src="man/figures/geometry-bubble-filled.png" width="220" alt="Same row with bubble A filled in pen"><br><sub>filled in</sub></td>
<td><img src="man/figures/geometry-bubble-marked.png" width="220" alt="Same row with A circled in orange by the marker, confirming the read"><br><sub>read back</sub></td>
</tr>
</table>

**4. Each bubble is compared with its own blank.** The printed letter inside a
circle is dark ink too, and different letters carry different amounts (a B has
more than an A). So `calibrate_coords()` also records how dark every bubble is
on the blank form, and a scan is read against that baseline: a bubble counts as
filled only if it is clearly darker than its own unfilled level. Then the
darkest bubble in a row wins, unless two are close. If two circles are both
dark and close in ink score, the row is ambiguous and gets flagged rather than
guessed at. The same ink-sampling approach reads the student-ID grid (one
column of digit bubbles per ID digit) and, when `zbar` can't decode the page's
QR code, falls back to matching the ink pattern against each version's
expected answer key to recover the version number. Every marked page is
written back out with its reading overlaid, so a human can audit the call at
a glance — orange for confident, red for anything flagged:

<img src="man/figures/geometry-marked-page.jpeg" width="640" alt="Full marked quiz page: filled zID grid, Q1/Q2/Q4 circled in orange, Q3 circled in red because a second bubble was also inked">

In this example sheet, Q3 has a stray second mark next to the intended
answer; both bubbles register enough ink to be ambiguous, so the row is
circled in red and flagged for review rather than scored automatically.

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

Place the scan PDF in the course folder, for example:

```text
scans.pdf
```

Then run:

```r
preprocess_scans("scans.pdf", "exam.yml", force = TRUE)
check_scan_sequence("scans")
mark_scans_cv("scans", config = "exam.yml", layout = "output/layout.R")
aggregate_sheets("scans", config = "exam.yml")
score_results("scans", key = "output/answer_key.csv", config = "exam.yml")
```

Outputs:

```text
scans/scan_sequence.csv   one row per page: version, page number, sheet
scans/progress.csv        one row per page, as read by the marker
scans/marked-cv/          each page with the recorded answers drawn on
scans/sheets.csv          one row per student, pages joined
scans/results.csv         scores
```

Rows needing manual review are flagged in `progress.csv` and `sheets.csv`.
`mark_scans_cv()` currently marks every CV-read page `needs_review` as a
conservative default — check the per-answer notes column (a trailing `*`
marks the bubble-level ambiguity that actually matters) rather than treating
every flagged row as equally uncertain.

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

Shorter questions fit better; longer questions may reduce the limit. Beyond
that, a quiz simply runs to more pages — `calibrate_coords()` and
`mark_scans_cv()` are page-aware, so a multi-page duplex form is marked the
same way a single-page one is, just with `check_scan_sequence()` and
`aggregate_sheets()` doing the extra bookkeeping of joining pages back into
one sheet per student (see above).

## External Tools

bubblequiz calls two external programs:

- **zbar** (`zbarimg`) decodes the QR codes on scanned pages.
- **XeLaTeX** with the TeX Live `qrcode` package typesets the printed forms.

On macOS (Homebrew, with [MacTeX](https://tug.org/mactex/) or BasicTeX):

```bash
brew install zbar
sudo tlmgr install qrcode   # if your TeX installation lacks the package
```

On Debian or Ubuntu:

```bash
sudo apt install zbar-tools texlive-xetex texlive-latex-extra
```

## Running The Tests

From a clone of the repository:

```r
devtools::test()
```

`tests/test_layout.R` is a regression check that the layout derived from the
example config matches the original hand-written BEES2041 tables exactly. It
runs as part of `R CMD check`, which is also what CI runs on every push and
pull request.

## License

bubblequiz is released under the MIT License. See [LICENSE.md](LICENSE.md).
