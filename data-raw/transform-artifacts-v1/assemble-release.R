entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(entry_file, mustWork = TRUE)), "scripts", "common.R"))

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop("Usage: Rscript assemble-release.R <qualified-candidate-dir> <release-dir>", call. = FALSE)
}
candidate_dir <- normalizePath(arguments[[1L]], mustWork = TRUE)
release_dir <- arguments[[2L]]
routes <- routes_definition()
policy <- read_json(file.path(artifact_root(), "qualification-policy.json"))
if (!isTRUE(policy$release_approval$approved) || is.null(policy$thresholds)) {
  stop("Cannot assemble a release from an unapproved qualification policy.", call. = FALSE)
}
if (dir.exists(release_dir) && length(list.files(release_dir, all.files = TRUE, no.. = TRUE))) {
  stop("Release directory already contains files: ", release_dir, call. = FALSE)
}
dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)

checksums <- character()
for (route in routes$routes) {
  for (name in c("forward", "inverse")) {
    file_name <- route$outputs[[name]]
    source <- file.path(candidate_dir, file_name)
    destination <- file.path(release_dir, file_name)
    if (!file.exists(source) || !file.copy(source, destination, copy.date = TRUE)) {
      stop("Missing or uncopyable qualified artifact: ", source, call. = FALSE)
    }
    checksums <- c(checksums, paste(sha256_file(destination), file_name))
  }
}
writeLines(checksums, file.path(release_dir, "SHA256SUMS"))
write_json(
  list(
    schema_version = 1,
    release = routes$artifact_release,
    policy = policy,
    files = strsplit(checksums, " ", fixed = TRUE)
  ),
  file.path(release_dir, "transform-artifacts-v1.json")
)
