#!/usr/bin/env Rscript
# Verify admitted public routes reproduce evaluated candidate coefficients.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
base <- args[1]
devtools::load_all(quiet = TRUE)
sha <- function(p) digest::digest(file = p, algo = 'sha256')
receipt <- jsonlite::read_json(file.path(base, 'consumer-receipt.json'))
operators <- lapply(list.files(file.path(base, 'cache/surface-operators-v1'),
  pattern = '[.]rds$', full.names = TRUE), readRDS)
reports <- list()
for (name in names(receipt$routes)) {
  parts <- strsplit(name, '_to_', fixed = TRUE)[[1]]
  geometry <- lapply(parts, function(part) {
    fields <- strsplit(part, '-', fixed = TRUE)[[1]]
    get_surface_geometry(fields[1], fields[2], fields[3],
      cache_dir = args[2], offline = TRUE)
  })
  from <- geometry[[1]]
  to <- geometry[[2]]
  op <- get_template_transform(from, to, cache_dir = file.path(base, 'public-cache'))
  replay <- get_template_transform(from, to,
    cache_dir = file.path(base, 'public-cache'), offline = TRUE)
  candidate <- Filter(function(x) identical(x$integrity,
    receipt$routes[[name]]$operator_id), operators)[[1]]
  fields <- setdiff(names(op$plan), 'timing')
  stopifnot(op$specification$qualification == 'passed', identical(op, replay),
    identical(op$plan[fields], candidate$plan[fields]), !op$specification$reversible)
  x <- surface_data(from$sphere[, 1]/100, from$domain)
  y <- apply_template_transform(x, op)
  expected <- apply_surface_transform(x, candidate)
  stopifnot(identical(y$values, expected$values),
    identical(y$coverage, expected$coverage), identical(y$domain$id, to$domain$id))
  reports[[name]] <- list(operator_id = op$integrity,
    exact_candidate_coefficients_equal = TRUE, offline_replay_equal = TRUE,
    public_application_equal = TRUE, qualification = op$specification$qualification)
  cat(name, 'PASS\n')
}
jsonlite::write_json(list(status = 'PASS',
  script_sha256 = sha('data-raw/fsaverage-density-v1/check-public-routes.R'),
  registry_sha256 = sha('inst/extdata/transform_registry.csv'),
  consumer_receipt_sha256 = sha(file.path(base, 'consumer-receipt.json')),
  routes = reports), file.path(base, 'public-receipt.json'),
  auto_unbox = TRUE, pretty = TRUE, digits = 17)
