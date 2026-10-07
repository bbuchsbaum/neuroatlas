#!/usr/bin/env Rscript
# Build exact descriptors only after input identities have been pinned.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
devtools::load_all(quiet = TRUE)
path <- 'inst/extdata/surface-density-inputs-v1.json'
lock <- jsonlite::read_json(path)
sha <- function(p) digest::digest(file = p, algo = 'sha256')
revision <- sha(path)
for (a in lock$assets) stopifnot(sha(file.path(args[1], a$path)) == a$sha256)
old <- Sys.getenv('NEUROATLAS_SURFACE_INPUTS')
catalog <- list(schema = 'neuroatlas.surface-domain-assets.v1',
  input_lock_sha256 = revision, domains = list())
checks <- list()
for (density in c('10k', '41k')) for (hemi in c('L', 'R')) {
  assets <- Filter(function(a) a$density == density && a$hemisphere == hemi,
    lock$assets)
  paths <- setNames(vapply(assets, function(a) a$path, character(1)),
    vapply(assets, function(a) a$role, character(1)))
  g <- gifti::readgii(file.path(args[1], paths[['sphere']]))
  ref <- gifti::readgii(file.path(args[1], paths[['ordering_reference']]))
  high <- gifti::readgii(file.path(old,
    paste0('tpl-fsaverage_hemi-', hemi, '_den-164k_sphere.surf.gii')))
  stopifnot(identical(g$data$pointset, ref$data$pointset),
    identical(g$data$triangle, ref$data$triangle),
    identical(g$data$pointset, high$data$pointset[seq_len(nrow(g$data$pointset)), ]))
  mask <- as.vector(gifti::readgii(file.path(args[1], paths[['mask']]))$data[[1]])
  area <- as.vector(gifti::readgii(file.path(args[1], paths[['area']]))$data[[1]])
  stopifnot(all(mask %in% c(0, 1)), all(is.finite(area)), all(area > 0))
  domain <- surface_domain('fsaverage', hemi, density, g$data$pointset,
    g$data$triangle, as.logical(mask), 'fsaverage', revision, vertex_area = area)
  name <- paste('fsaverage', density, hemi, sep = '-')
  catalog$domains[[name]] <- list(domain = domain,
    assets = as.list(paths[c('sphere', 'mask', 'area')]))
  checks[[name]] <- list(n_vertices = domain$n_vertices,
    n_triangles = domain$n_triangles, cortex_vertices = sum(mask),
    upstream_ordering_equal = TRUE, high_density_prefix_equal = TRUE,
    domain_id = domain$id)
  cat(name, domain$id, '\n')
}
jsonlite::write_json(catalog, 'inst/extdata/surface-density-domains-v1.json',
  auto_unbox = TRUE, pretty = TRUE, digits = 17)
jsonlite::write_json(list(status = 'PASS', input_lock_sha256 = revision,
  script_sha256 = sha('data-raw/fsaverage-density-v1/prepare-domains.R'),
  checks = checks), 'data-raw/fsaverage-density-v1/input-ordering-receipt.json',
  auto_unbox = TRUE, pretty = TRUE, digits = 17)
