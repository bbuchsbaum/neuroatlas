#!/usr/bin/env Rscript
# Run from the package root after qualifying the current public API.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L, !dir.exists(args[[1L]]))
out <- args[[1L]]
dir.create(out, recursive = TRUE)
devtools::load_all(quiet = TRUE)
base <- "data-raw/surface-transforms-v1"
cache <- file.path(base, "work/public-surface-cache")
inputs <- Sys.getenv("NEUROATLAS_SURFACE_INPUTS")
lock <- jsonlite::read_json(file.path(base, "inputs.lock.json"))
sha <- function(path) digest::digest(file = path, algo = "sha256")
atlas_path <- file.path(base, "work/harvard-oxford-cortex.rds")
atlas <- if (file.exists(atlas_path)) readRDS(atlas_path) else {
  get_harvard_oxford_atlas("cortical", threshold = 25, resolution = "02")
}
cases <- list()
areas <- list()
for (frame in c("MNI152NLin6Asym", "MNI152NLin2009cAsym")) {
  volume <- get_template(frame, resolution = 2)
  stopifnot(identical(template_metadata(volume)$spatial$template_space, frame))
  for (hemi in c("L", "R")) for (template in c("fsaverage", "fsLR")) {
    density <- if (template == "fsaverage") "164k" else "32k"
    target <- get_surface_geometry(template, density, hemi,
      cache_dir = cache, offline = TRUE)
    projection <- get_template_transform(frame, target,
      cache_dir = cache, offline = TRUE)
    scalar <- apply_template_transform(volume, projection,
      data_type = "continuous")
    replay <- apply_template_transform(volume,
      get_template_transform(frame, target, cache_dir = cache, offline = TRUE),
      data_type = "continuous")
    stopifnot(identical(scalar, replay), all(is.finite(scalar$values[
      scalar$coverage$available])))
    name <- paste(frame, template, hemi, sep = "-")
    saveRDS(scalar, file.path(out, paste0(name, ".rds")))
    writeBin(as.double(scalar$values), file.path(out, paste0(name, ".bin")),
      size = 8L, endian = "little")
    writeBin(as.double(projection$points[[1L]]),
      file.path(out, paste0(name, "-sampling.bin")), size = 8L, endian = "little")
    cases[[name]] <- list(frame = frame, hemisphere = hemi, template = template,
      domain = target$domain$id, available = sum(scalar$coverage$available),
      cortical_vertices = sum(target$cortex),
      value_range = range(scalar$values, na.rm = TRUE),
      source_values_sha256 = scalar$provenance$source_values_sha256,
      projection_id = projection$id, offline_replay_identical = TRUE)
    if (frame == "MNI152NLin6Asym") {
      labels <- transform_atlas(atlas, target, cache_dir = cache, offline = TRUE)
      stopifnot(identical(labels$provenance$source_atlas_ref, atlas_ref(atlas)))
      writeBin(as.double(labels$values), file.path(out, paste0(name, "-labels.bin")),
        size = 8L, endian = "little")
      saveRDS(labels, file.path(out, paste0(name, "-labels.rds")))
      keys <- sort(unique(as.vector(labels$values[is.finite(labels$values)])))
      area_asset <- Filter(function(a) identical(a$template, template) &&
        identical(a$hemisphere, hemi) && identical(a$role, "area"), lock$assets)
      stopifnot(length(area_asset) == 1L)
      area_path <- file.path(inputs, area_asset[[1L]]$path)
      stopifnot(identical(sha(area_path), area_asset[[1L]]$sha256))
      area <- as.numeric(gifti::readgii(area_path)$data[[1L]])
      areas[[name]] <- data.frame(template = template, hemisphere = hemi,
        key = keys, name = labels$label_table$name[
          match(keys, labels$label_table$key)],
        vertices = vapply(keys, function(key) sum(labels$values == key,
          na.rm = TRUE), integer(1)),
        area_mm2 = vapply(keys, function(key) sum(area[
          is.finite(labels$values) & labels$values == key]), numeric(1)))
      cases[[name]]$label_keys <- keys
      cases[[name]]$lost_labels <- setdiff(atlas$ids, keys)
      cases[[name]]$source_atlas_ref <- atlas_ref(atlas)
      cases[[name]]$vertex_area_sha256 <- sha(area_path)
    }
    cat(name, "PASS", cases[[name]]$available, "available vertices\n")
  }
}
write.csv(do.call(rbind, areas), file.path(out, "label-areas.csv"), row.names = FALSE)
receipt <- list(status = "PASS", scope = "actual T1w maps and Harvard-Oxford labels",
  anatomical_accuracy = "not established; dependent population labels",
  driver_sha256 = sha(file.path(base, "projection-v1/public-examples.R")),
  cases = cases,
  files = as.list(setNames(vapply(list.files(out, full.names = TRUE), sha,
    character(1)), list.files(out))))
jsonlite::write_json(receipt, file.path(out, "public-examples-receipt.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 17)
