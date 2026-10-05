# Six-Question Quiz Example

This directory is a complete small course-repo-style example for `bubblequiz`.

## Source Files

- `exam.yml`: quiz shape, versions, ID format, and layout.
- `questions.md`: master question source with answer comments.

## Generated Outputs

- `output/questions_v1.md` ... `output/questions_v4.md`: randomized question text for each version.
- `output/quizform_v1.pdf` ... `output/quizform_v4.pdf`: printable/scannable forms, one version per PDF.
- `output/quizforms_all_versions.pdf`: one combined print file containing all versions.
- `output/answer_key.csv`: version-aware answer key used for scoring.
- `output/answer_key.md`: human-readable version mappings.

## Rebuild

From this directory:

```r
library(bubblequiz)
generate_versions("exam.yml", "questions.md", "output")
make_quiz_forms("exam.yml", "output")
combine_quiz_forms("exam.yml", "output")
```

The combined PDF is designed for duplex printing. In this example, each version is two pages, so each student receives one double-sided sheet.

Note: this example demonstrates question/version generation and the combined
print PDF, not a full scan-and-mark run. See the main README's "How The
Geometry Pipeline Works" section for how a printed page like this one is
calibrated and read back; `calibrate_coords()` and `mark_scans_cv()` are
page-aware, so this two-page form is marked live the same way a one-page quiz
is.
