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
for (f in list.files("../../R", "[.]R$", full.names = TRUE)) source(f)
generate_versions("exam.yml", "questions.md", "output")
make_quiz_forms("exam.yml", "output")
combine_quiz_forms("exam.yml", "output")
```

The combined PDF is designed for duplex printing. In this example, each version is two pages, so each student receives one double-sided sheet.

Note: this example demonstrates question/version generation and the combined
print PDF. The current local CV marker is calibrated per physical page, so use a
one-page-per-version quiz for live marking until multi-page marking is added.
