arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 4L) stop("Usage: validate-qualification-output.R <qa.json> <report.html> <visual-qa.json> <visual-index.html>", call. = FALSE)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required for qualification validation.", call. = FALSE)
for (path in arguments) {
  if (!file.exists(path) || file.info(path)$size < 1L) stop("Missing or empty output: ", path, call. = FALSE)
}
qa <- jsonlite::read_json(arguments[[1L]], simplifyVector = FALSE)
if (!isTRUE(qa$release_eligible)) stop("Qualification evidence is not release-eligible.", call. = FALSE)
visual <- jsonlite::read_json(arguments[[3L]], simplifyVector = FALSE)
if (!isTRUE(visual$qualitative_only) || length(visual$files) < 5L) {
  stop("Visual QA manifest is incomplete or claims to be quantitative evidence.", call. = FALSE)
}
for (receipt in visual$files) {
  path <- file.path(dirname(arguments[[3L]]), receipt$path)
  if (!file.exists(path) || file.info(path)$size != receipt$bytes) {
    stop("Visual QA image is missing or changed: ", path, call. = FALSE)
  }
}
