#!/usr/bin/env Rscript
# Post hoc localization of parcel loss to projection or mesh resampling.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L, !dir.exists(args[[2L]]))
work <- args[[1L]]
out <- args[[2L]]
dir.create(out)
devtools::load_all(quiet = TRUE)
cache <- file.path(work, "cache")
path <- file.path(work, "inputs",
  "Schaefer2018_1000Parcels_7Networks_order_FSLMNI152_1mm.nii.gz")
volume <- neuroim2::read_vol(path)
labels <- jsonlite::read_json(file.path(work, "surface-inputs",
  "labels-1000.json"), simplifyVector = TRUE)
table <- data.frame(key = as.numeric(names(labels)), name = as.character(labels))
cases <- list()
for (hemi in c("L", "R")) {
  geometry <- get_surface_geometry("fsaverage", "164k", hemi,
    cache_dir = cache, offline = TRUE)
  projection <- get_template_transform("MNI152NLin6Asym", geometry,
    cache_dir = cache, offline = TRUE)
  result <- apply_surface_projection(volume, projection,
    source_space = "MNI152NLin6Asym", data_type = "label", label_table = table)
  writeBin(as.double(result$values), file.path(out, paste0(hemi, "-values.bin")),
    size = 8L, endian = "little")
  writeBin(as.double(projection$points[[1L]]),
    file.path(out, paste0(hemi, "-points.bin")), size = 8L, endian = "little")
  cases[[hemi]] <- list(projection_id = projection$id,
    observed = sort(unique(as.numeric(result$values[is.finite(result$values)]))))
}
sha <- function(path) digest::digest(file = path, algo = "sha256")
jsonlite::write_json(list(post_hoc = TRUE, source_sha256 = sha(path),
  script_sha256 = sha("data-raw/transform-quality-v1/projection-stages.R"),
  cases = cases, files = as.list(setNames(
    vapply(list.files(out, full.names = TRUE), sha, character(1)), list.files(out)))),
  file.path(out, "receipt.json"), pretty = TRUE, auto_unbox = TRUE)
