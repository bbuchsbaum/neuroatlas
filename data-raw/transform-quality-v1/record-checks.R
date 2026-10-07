#!/usr/bin/env Rscript
work <- commandArgs(TRUE)[[1L]]
library(testthat)
tests <- as.data.frame(readRDS(file.path(work, "package-tests.rds")))
check <- readRDS(file.path(work, "package-check.rds"))
summary <- list(
  tests = as.list(colSums(tests[, c("failed", "error", "warning", "skipped", "passed")])),
  test_console_summary = tail(grep("\\[ FAIL", readLines(
    file.path(work, "package-tests.log")), value = TRUE), 1L),
  check = list(errors = check$errors, warnings = check$warnings, notes = check$notes),
  tests_log_sha256 = digest::digest(file = file.path(work, "package-tests.log"), algo = "sha256"),
  check_log_sha256 = digest::digest(file = file.path(work, "package-check.log"), algo = "sha256")
)
stopifnot(summary$tests$failed == 0, summary$tests$error == 0,
  length(summary$check$errors) == 0, length(summary$check$warnings) == 0)
jsonlite::write_json(summary, file.path(work, "checks.json"),
  pretty = TRUE, auto_unbox = TRUE)
