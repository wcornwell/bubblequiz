# Rebuild the static 22-page scan fixture from the worked example.  Run from
# the package root with:
#
#   Rscript tests/testthat/fixtures/synthetic-scan/generate.R
#
# Everything on these pages is generated: the forms, zIDs, marks, scanner
# shifts and rotations.  No student work or personal data is used.

pkgload::load_all(".", quiet = TRUE)
source("tests/testthat/helper-scans.R", local = TRUE)

fixture <- "tests/testthat/fixtures/synthetic-scan"
forms <- "example/six-question-quiz/output"
cfg <- load_exam_config("example/six-question-quiz/exam.yml")
work <- tempfile("synthetic-scan-source-")
dir.create(work)

truth <- list(
  list(version = "1", zid = "9000001", answers = c("A", "C", "A", "B", "A", "B"),
       distort = list(dx_mm = 0.5, dy_mm = 0.5)),
  list(version = "2", zid = "9000002", answers = c("C", "A", "C", "E", "C", "E"),
       distort = list(dx_mm = -0.5, dy_mm = 1)),
  list(version = "3", zid = "9000003", answers = c("D", "B", "D", "C", "D", "C"),
       distort = list(deg = 0.25)),
  list(version = "4", zid = "9000004", answers = c("E", "A", "E", "D", "E", "D"),
       distort = list(dx_mm = 1, dy_mm = -0.5)),
  list(version = "1", zid = "9000005", answers = c("B", "D", "C", "A", "E", "B"),
       distort = list(dx_mm = -1)),
  list(version = "2", zid = "9000006", answers = c("D", "B", "A", "C", "E", "D"),
       distort = list(dy_mm = 1)),
  list(version = "3", zid = "9000007", answers = c("E", "C", "B", "D", "A", "E"),
       distort = list(dx_mm = 0.5, deg = -0.2)),
  list(version = "4", zid = "9000008", answers = c("A", "D", "E", "B", "C", "A"),
       distort = list(dy_mm = -1)),
  list(version = "1", zid = "9000009", answers = c("C", "A", "D", "E", "B", "C"),
       distort = list(dx_mm = 1, dy_mm = 0.5)),
  list(version = "2", zid = "9000010", answers = c("B", "E", "C", "A", "D", "B"),
       distort = list(dx_mm = -0.5, deg = 0.2)),
  list(version = "3", zid = "9000011", answers = c("D", "C", "E", "B", "A", "D"),
       distort = list(dx_mm = 0.5, dy_mm = -0.5))
)

pages <- unlist(lapply(seq_along(truth), function(i) {
  x <- truth[[i]]
  make_sheet(forms, cfg, x$version, x$zid, x$answers, work, i,
             distort = x$distort)
}))
write_scan_pdf(pages, file.path(fixture, "scan.pdf"))

expected <- data.frame(
  sheet = seq_along(truth),
  zid = paste0("z", vapply(truth, `[[`, "", "zid")),
  version = vapply(truth, `[[`, "", "version"),
  answers = vapply(truth, function(x) paste(x$answers, collapse = ""), "")
)
utils::write.csv(expected, file.path(fixture, "expected.csv"), row.names = FALSE,
                 quote = TRUE)
