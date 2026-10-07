#!/usr/bin/env Rscript
# Exercise full-density all-source and missingness policies with pinned engine.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
base <- args[1]
devtools::load_all(quiet = TRUE)
sha <- function(p) digest::digest(file = p, algo = 'sha256')
receipt <- jsonlite::read_json(file.path(base, 'consumer-receipt.json'))
contract <- jsonlite::read_json('data-raw/fsaverage-density-v1/contract-v1.json')
stopifnot(sha('data-raw/fsaverage-density-v1/contract-v1.json') == receipt$contract_sha256,
  neuroatlas:::.surface_engine_revision_verified(contract$engine_revision))
operators <- lapply(list.files(file.path(base, 'cache/surface-operators-v1'),
  pattern = '[.]rds$', full.names = TRUE), readRDS)
reports <- list()
for (name in names(receipt$routes)) {
  op <- Filter(function(x) identical(x$integrity,
    receipt$routes[[name]]$operator_id), operators)[[1]]
  plan <- op$plan
  ns <- plan$n_moving
  nt <- plan$n_reference
  # All-source test changes a read-only plan copy, not an admitted domain.
  all_source <- plan
  all_source$source_mask <- rep(TRUE, ns)
  all_source$target_mask <- rep(TRUE, nt)
  result <- neurotransform::apply_surface_resampling(all_source,
    cbind(one = rep(1, ns), bounded = sin(seq_len(ns) * 0.12345)), details = TRUE)
  stopifnot(all(result$available),
    max(abs(result$values[, 1] - 1)) <= contract$bounded_value_tolerance,
    max(abs(result$values[, 2])) <= 1 + contract$bounded_value_tolerance)
  baseline <- neurotransform::apply_surface_resampling(plan, rep(0, ns), details = TRUE)
  stopifnot(all(baseline$values[baseline$available] == 0))
  candidate <- which(plan$source_mask[plan$cols] & plan$target_mask[plan$rows])[1]
  missing_column <- plan$cols[candidate]
  x <- rep(0.25, ns)
  x[missing_column] <- NA_real_
  propagated <- neurotransform::apply_surface_resampling(plan, x,
    na_policy = 'propagate', details = TRUE)
  omitted <- neurotransform::apply_surface_resampling(plan, x,
    na_policy = 'omit', details = TRUE)
  affected <- unique(plan$rows[plan$cols == missing_column & plan$vals > 0])
  expected <- baseline$available
  expected[affected] <- FALSE
  stopifnot(identical(propagated$available, expected),
    all(is.na(propagated$values[!expected])),
    max(abs(propagated$values[expected] - 0.25)) <= 1e-12)
  active <- plan$source_mask[plan$cols] & plan$cols != missing_column
  surviving <- tabulate(plan$rows[active], nbins = nt) > 0 & plan$target_mask
  stopifnot(identical(omitted$available, surviving),
    all(is.na(omitted$values[!surviving])),
    max(abs(omitted$values[surviving] - 0.25)) <= 1e-12,
    inherits(try(neurotransform::apply_surface_resampling(plan, x,
      na_policy = 'error'), silent = TRUE), 'try-error'))
  reports[[name]] <- list(all_source_rows = nt, supported_zero_pass = TRUE,
    missing_column = missing_column, affected_rows = length(affected),
    propagate_omit_error_pass = TRUE)
  cat(name, 'PASS\n')
}
jsonlite::write_json(list(status = 'PASS', contract_sha256 = receipt$contract_sha256,
  consumer_receipt_sha256 = sha(file.path(base, 'consumer-receipt.json')),
  script_sha256 = sha('data-raw/fsaverage-density-v1/check-policies.R'), routes = reports),
  file.path(base, 'policy-receipt.json'), auto_unbox = TRUE, pretty = TRUE, digits = 17)
