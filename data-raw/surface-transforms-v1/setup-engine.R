#!/usr/bin/env Rscript
# Install the locked engine into a caller-owned library and bind the fresh build.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
for (package in c("digest", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install development dependency: ", package)
  }
}
library <- args[[1L]]
dir.create(library, recursive = TRUE, showWarnings = FALSE)
library <- normalizePath(library, mustWork = TRUE)
revision <- "933edddda462593941e167726e8aaa7168ff103a"
archive_sha <- "a3650c35f11788f128740de360816eab36c9b4acac046c52f44e533bb65e5fa2"
archive <- tempfile(fileext = ".tar.gz")
utils::download.file(paste0(
  "https://codeload.github.com/bbuchsbaum/neurotransform/tar.gz/", revision),
  archive, mode = "wb", quiet = TRUE)
sha <- function(path) digest::digest(file = path, algo = "sha256")
stopifnot(identical(sha(archive), archive_sha))
status <- system2(file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", shQuote(paste0("--library=", library)), shQuote(archive)))
if (status != 0L) stop("Pinned engine installation failed: ", status)
root <- file.path(library, "neurotransform")
files <- list.files(root, recursive = TRUE, full.names = TRUE)
relative <- substring(files, nchar(root) + 2L)
receipt <- list(schema = "neuroatlas.engine-build.v1",
  engine_revision = revision, archive_sha256 = archive_sha,
  version = read.dcf(file.path(root, "DESCRIPTION"))[1L, "Version"],
  installed_artifacts_sha256 = as.list(setNames(
    vapply(files, sha, character(1)), relative)))
receipt_path <- file.path(library, "engine-build.json")
jsonlite::write_json(receipt, receipt_path, pretty = TRUE, auto_unbox = TRUE)
unlink(archive)
cat("Engine build receipt:", receipt_path, "\n")
