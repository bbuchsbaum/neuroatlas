entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(entry_file, mustWork = TRUE)), "scripts", "common.R"))

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 4L) {
  stop(
    "Usage: Rscript materialize-nibi-campaign.R <template> <output> <remote-root> <output-root>",
    call. = FALSE
  )
}

template <- normalizePath(arguments[[1L]], mustWork = TRUE)
output <- arguments[[2L]]
remote_root <- arguments[[3L]]
output_root <- arguments[[4L]]
if (!grepl("^/", remote_root) || !grepl("^/", output_root) ||
    grepl("__NIBI_", remote_root, fixed = TRUE) ||
    grepl("__NIBI_", output_root, fixed = TRUE)) {
  stop("Remote and output roots must be concrete absolute paths, not sentinels.", call. = FALSE)
}
if (file.exists(output)) {
  stop("Refusing to overwrite generated campaign: ", output, call. = FALSE)
}
if (normalizePath(dirname(output), mustWork = TRUE) != dirname(template)) {
  stop(
    "Output must be a sibling of the template so relative campaign scripts ",
    "and validators remain resolvable.",
    call. = FALSE
  )
}

contents <- paste(readLines(template, warn = FALSE), collapse = "\n")
contents <- gsub("__NIBI_SMOKE_REMOTE_ROOT__|__NIBI_REMOTE_ROOT__", remote_root, contents)
contents <- gsub("__NIBI_SMOKE_OUTPUT_ROOT__|__NIBI_OUTPUT_ROOT__", output_root, contents)
if (grepl("__NIBI_", contents, fixed = TRUE)) {
  stop("Generated campaign still contains an Nibi sentinel.", call. = FALSE)
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
writeLines(contents, output)
message("Materialized campaign at ", normalizePath(output, mustWork = TRUE))
