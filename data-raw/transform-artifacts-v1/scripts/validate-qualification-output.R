entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_dir <- if (length(entry_file) == 1L) {
  dirname(normalizePath(entry_file, mustWork = TRUE))
} else {
  normalizePath(file.path(getwd(), "data-raw", "transform-artifacts-v1", "scripts"), mustWork = TRUE)
}
source(file.path(script_dir, "common.R"))
source(file.path(script_dir, "qualification-gates.R"))

validate_qualification_output <- function(qa_path, report_path, visual_manifest_path, visual_report_path) {
  for (path in c(qa_path, report_path, visual_manifest_path, visual_report_path)) {
    if (!file.exists(path) || file.info(path)$size < 1L) stop("Missing or empty output: ", path, call. = FALSE)
  }
  qa <- read_json(qa_path)
  policy_path <- file.path(dirname(script_dir), "qualification-policy.json")
  policy <- read_json(policy_path)
  if (!identical(qa$policy_sha256, sha256_file(policy_path))) stop("qa policy_sha256 does not match policy file.", call. = FALSE)
  verdict <- evaluate_qualification(qa, policy)
  if (!verdict$passed) stop(paste(verdict$failures, collapse = "; "), call. = FALSE)
  bindings <- validate_qualification_bindings(qa)
  if (length(bindings)) stop(paste(bindings, collapse = "; "), call. = FALSE)
  visual <- read_json(visual_manifest_path)
  if (!isTRUE(visual$qualitative_only) || !is.list(visual$files) || length(visual$files) < 5L) {
    stop("Visual QA manifest is incomplete or claims quantitative evidence.", call. = FALSE)
  }
  for (receipt in visual$files) {
    path <- file.path(dirname(visual_manifest_path), receipt$path)
    if (!qualification_receipt_matches(path, receipt)) stop("Visual QA file is missing or changed: ", path, call. = FALSE)
  }
  invisible(verdict)
}

if (sys.nframe() == 0L && !interactive()) {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) != 4L) stop("Usage: validate-qualification-output.R <qa.json> <report.html> <visual-qa.json> <visual-index.html>", call. = FALSE)
  validate_qualification_output(arguments[[1L]], arguments[[2L]], arguments[[3L]], arguments[[4L]])
}
