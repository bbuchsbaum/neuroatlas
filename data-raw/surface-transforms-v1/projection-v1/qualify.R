#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L, !dir.exists(args[[1L]]))
out <- args[[1L]]
dir.create(out, recursive = TRUE)
base <- "data-raw/surface-transforms-v1/projection-v1"
sha <- function(path) digest::digest(file = path, algo = "sha256")
sources <- c("DESCRIPTION", "NAMESPACE", list.files("R", full.names = TRUE,
                                                     pattern = "[.]R$"),
  file.path("inst/extdata", c("transform_registry.csv", "surface-inputs-v1.json",
    "surface-domains-v1.json", "projection-inputs-v1.json")))
source_hashes <- as.list(setNames(vapply(sources, sha, character(1)), sources))
driver_hash <- sha(file.path(base, "qualify.R"))
devtools::load_all(quiet = TRUE)
contract <- jsonlite::read_json(file.path(base, "contract-v1.json"))
stopifnot(neuroatlas:::.surface_engine_revision_verified(contract$engine_revision))
write_array <- function(x, path, integer = FALSE) {
  connection <- file(path, "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(if (integer) as.integer(t(x)) else as.double(t(x)), connection,
    size = if (integer) 4L else 8L, endian = "little")
}
write_volume <- function(values, name) {
  path <- file.path(out, paste0(name, ".bin"))
  writeBin(as.double(values), path, size = 8L, endian = "little")
  path
}
cases <- list()
export_case <- function(name, projection, values, affine, volume_path, kind, policy,
                        result, extra = list()) {
  folder <- file.path(out, name)
  dir.create(folder)
  for (i in seq_along(projection$points)) {
    write_array(projection$points[[i]], file.path(folder, paste0("points-", i, ".bin")))
  }
  write_array(matrix(projection$cortex), file.path(folder, "cortex.bin"), TRUE)
  write_array(as.matrix(result$values), file.path(folder, "values.bin"))
  write_array(result$coverage$available, file.path(folder, "available.bin"), TRUE)
  write_array(result$coverage$source_weight_mass, file.path(folder, "mass.bin"))
  if (!is.null(projection$surface_operator)) {
    p <- projection$surface_operator$plan
    write_array(cbind(p$rows - 1L, p$cols - 1L, p$vals),
                file.path(folder, "surface-weights.bin"))
    write_array(matrix(p$target_mask), file.path(folder, "target-mask.bin"), TRUE)
    write_array(matrix(p$source_mask), file.path(folder, "source-mask.bin"), TRUE)
  }
  manifest <- c(list(name = name, specification = projection$specification,
    projection_id = projection$id, nodes = length(projection$points),
    source_vertices = projection$specification$sampling_domain$n_vertices,
    target_vertices = projection$specification$target$n_vertices,
    maps = NCOL(result$values), volume = basename(volume_path),
    volume_sha256 = sha(volume_path), dimensions = dim(values), affine = affine,
    data_type = kind, na_policy = policy,
    files = as.list(setNames(vapply(list.files(folder, full.names = TRUE), sha,
      character(1)), list.files(folder)))), extra)
  jsonlite::write_json(manifest, file.path(folder, "case.json"), pretty = TRUE,
                       auto_unbox = TRUE, digits = 17, matrix = "rowmajor")
  cases[[length(cases) + 1L]] <<- name
}

# Analytic and asymmetric synthetic point/ribbon cases include closed-box
# boundaries, exact nearest ties and a tiny positive contributor.
set.seed(contract$acceptance$seed)
n <- contract$acceptance$random_synthetic_points + 8L
sphere <- matrix(rnorm(n * 3L), ncol = 3L)
sphere <- sphere / sqrt(rowSums(sphere^2))
mask <- seq_len(n) %% 7L != 0L
domain <- surface_domain("oracle", "L", paste0(n, "v"), sphere,
  matrix(c(0, 1, 2), nrow = 1L), mask, "analytic", "projection-v1")
dims <- c(8L, 9L, 10L)
index <- arrayInd(seq_len(prod(dims)), dims) - 1L
voxel <- cbind(runif(n, 0, 7), runif(n, 0, 8), runif(n, 0, 9))
voxel[1:8, ] <- rbind(c(0, 0, 0), c(7, 8, 9), c(1.5, 2.5, 3.5),
  c(1e-8, 0, 0), c(-1e-8, 2, 3), c(7 + 1e-8, 2, 3),
  c(2, 3, 4), c(2, 3, 4))
affine <- rbind(c(0, -2, 0.1, 20), c(1.5, 0, 0.2, -30),
                c(0, 0.1, 3, 7), c(0, 0, 0, 1))
coords <- (cbind(voxel, 1) %*% t(affine))[, 1:3]
white <- coords
pial_voxel <- voxel
pial_voxel[, 1] <- pial_voxel[, 1] + 0.8
pial <- (cbind(pial_voxel, 1) %*% t(affine))[, 1:3]
scalar <- array(index[, 1] + 2 * index[, 2] + 4 * index[, 3], dims)
labels <- array(ifelse(index[, 1] < 3, 0, ifelse(index[, 2] < 4, 11, 12)), dims)
probability <- array(c(rep(0.25, prod(dims)), rep(0.5, prod(dims))), c(dims, 2L))
for (missing in c(FALSE, TRUE)) {
  for (kind in c("continuous", "label", "probability")) {
    values <- switch(kind, continuous = scalar, label = labels,
                     probability = probability)
    if (missing) {
      if (kind == "probability") values[1, 1, 1, 1] <- NA_real_ else {
        values[1, 1, 1] <- NA_real_
      }
    }
    volume_path <- write_volume(values, paste("synthetic", kind, missing, sep = "-"))
    for (nodes in c(2L, 3L, 5L, 9L)) {
      projection <- ribbon_projection(domain, white,
        if (nodes == 2L) white else pial, mask, "analytic-RAS", nodes)
      for (policy in if (missing) c("propagate", "omit") else {
        c("propagate", "omit", "error")
      }) {
        name <- paste("synthetic", kind, missing, nodes, policy, sep = "-")
        result <- apply_surface_projection(values, projection, "analytic-RAS",
          affine, kind, policy)
        export_case(name, projection, values, affine, volume_path, kind, policy, result)
      }
    }
  }
}

# Every advertised exact population route, on both pinned 1 and 2 mm grids.
grids <- jsonlite::read_json(file.path(base, "grids.lock.json"))
cache <- file.path(dirname(out), "public-surface-cache")
candidate <- neuroatlas:::.surface_input_json("projection-inputs-v1.json")$
  qualification != "passed"
projections <- list()
for (frame in c("MNI152NLin6Asym", "MNI152NLin2009cAsym")) {
  for (hemi in c("L", "R")) for (template in c("fsaverage", "fsLR")) {
    density <- if (template == "fsaverage") "164k" else "32k"
    target <- get_surface_geometry(template, density, hemi, cache)
    key <- paste(frame, template, hemi, sep = "-")
    projection <- if (candidate) {
      neuroatlas:::.registration_fusion_projection(frame, target, cache, TRUE, FALSE)
    } else get_template_transform(frame, target, cache_dir = cache)
    projections[[key]] <- projection
    replay <- if (candidate) {
      neuroatlas:::.registration_fusion_projection(frame, target, cache, FALSE, TRUE)
    } else get_template_transform(frame, target, cache_dir = cache, offline = TRUE)
    stopifnot(identical(projection, replay))
    saveRDS(projection, file.path(out, paste0(key, ".rds")))
  }
}
for (grid_key in names(grids)) {
  grid <- grids[[grid_key]]
  dims <- as.integer(unlist(grid$shape))
  affine <- do.call(rbind, lapply(grid$affine, unlist))
  index <- arrayInd(seq_len(prod(dims)), dims) - 1L
  xyz <- (cbind(index, 1) %*% t(affine))[, 1:3]
  ramp <- array(0.4 + xyz[, 1] / 1000 + xyz[, 2] / 2000 + xyz[, 3] / 3000, dims)
  labels <- array(ifelse(xyz[, 3] > 30, 0,
    ifelse(xyz[, 1] > 0, 11, 12)), dims)
  rm(index, xyz)
  gc()
  for (kind in c("continuous", "label", "probability")) {
    values <- switch(kind, continuous = ramp, label = labels,
      probability = array(c(as.vector(ramp), rep(0.25, prod(dims))), c(dims, 2L)))
    volume_path <- write_volume(values, paste(grid_key, kind, sep = "-"))
    for (hemi in c("L", "R")) for (template in c("fsaverage", "fsLR")) {
      key <- paste(grid$template, template, hemi, sep = "-")
      projection <- projections[[key]]
      name <- paste(grid_key, template, hemi, kind, sep = "-")
      cat("Evaluate", name, "\n")
      result <- apply_surface_projection(values, projection, grid$template,
        affine, kind)
      export_case(name, projection, values, affine, volume_path, kind, "propagate",
                  result, list(grid_input = grid))
    }
  }
  rm(ramp, labels, values)
  gc()
}
stopifnot(identical(source_hashes,
  as.list(setNames(vapply(sources, sha, character(1)), sources))),
  identical(driver_hash, sha(file.path(base, "qualify.R"))))
receipt <- list(status = "candidate output; independent oracle pending",
  execution = if (candidate) "private candidate builder" else "public template workflow",
  contract_sha256 = sha(file.path(base, "contract-v1.json")),
  source_sha256 = source_hashes, driver_sha256 = driver_hash,
  engine_build_sha256 = sha(Sys.getenv("NEUROATLAS_ENGINE_BINDING")),
  grids_lock_sha256 = sha(file.path(base, "grids.lock.json")),
  cases = cases,
  case_sha256 = as.list(setNames(vapply(cases, function(name) {
    sha(file.path(out, name, "case.json"))
  }, character(1)), cases)))
jsonlite::write_json(receipt, file.path(out, "consumer-receipt.json"),
                     pretty = TRUE, auto_unbox = TRUE, digits = 17)
