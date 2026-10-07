#!/usr/bin/env Rscript
# Export actual public API outputs for independent metrics and Workbench checks.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L, !dir.exists(args[[2L]]))
work <- args[[1L]]
out <- args[[2L]]
dir.create(out, recursive = TRUE)
devtools::load_all(quiet = TRUE)
cache <- file.path(work, "cache")
cases <- list()
sha <- function(path) digest::digest(file = path, algo = "sha256")
emit <- function(name, result) {
  writeBin(as.double(result$values), file.path(out, paste0(name, ".bin")),
    size = 8L, endian = "little")
  writeBin(as.integer(result$coverage$available),
    file.path(out, paste0(name, "-available.bin")), size = 4L, endian = "little")
}
for (hemi in c("L", "R")) {
  geometries <- list(
    fsaverage = get_surface_geometry("fsaverage", "164k", hemi,
      cache_dir = cache, offline = TRUE),
    fsLR = get_surface_geometry("fsLR", "32k", hemi,
      cache_dir = cache, offline = TRUE)
  )
  for (template in names(geometries)) {
    writeBin(as.integer(geometries[[template]]$cortex),
      file.path(out, paste0(template, "-", hemi, "-cortex.bin")),
      size = 4L, endian = "little")
  }
  for (from in names(geometries)) {
    to <- setdiff(names(geometries), from)
    operator <- get_template_transform(geometries[[from]], geometries[[to]],
      cache_dir = cache, offline = TRUE)
    for (scale in c(100, 400, 1000)) {
      name <- paste(from, to, hemi, scale, sep = "-")
      path <- file.path(work, "surface-inputs",
        paste0(from, "-", hemi, "-", scale, ".label.gii"))
      x <- surface_data(path, geometries[[from]]$domain, "label")
      result <- apply_template_transform(x, operator)
      emit(name, result)
      cases[[name]] <- list(from = from, to = to, hemi = hemi, scale = scale,
        data_type = "label", input_sha256 = sha(path),
        operator_id = operator$integrity, provenance = result$provenance)
      cat(name, "exported\n")
    }
    if (from == "fsaverage") for (kind in c("sulc", "curv")) {
      name <- paste(from, to, hemi, kind, sep = "-")
      path <- file.path(work, "inputs",
        paste0("tpl-fsaverage_hemi-", hemi, "_den-164k_", kind, ".shape.gii"))
      result <- apply_template_transform(
        surface_data(path, geometries[[from]]$domain), operator)
      emit(name, result)
      cases[[name]] <- list(from = from, to = to, hemi = hemi, kind = kind,
        data_type = "continuous", input_sha256 = sha(path),
        operator_id = operator$integrity)
    }
  }
  projection <- get_template_transform("MNI152NLin6Asym", geometries$fsLR,
    cache_dir = cache, offline = TRUE)
  for (scale in c(100, 400, 1000)) {
    labels <- jsonlite::read_json(file.path(work, "surface-inputs",
      paste0("labels-", scale, ".json")), simplifyVector = TRUE)
    label_table <- data.frame(key = as.numeric(names(labels)),
      name = as.character(labels))
    path <- file.path(work, "inputs", paste0(
      "Schaefer2018_", scale,
      "Parcels_7Networks_order_FSLMNI152_1mm.nii.gz"))
    result <- apply_surface_projection(neuroim2::read_vol(path), projection,
      source_space = "MNI152NLin6Asym", data_type = "label",
      label_table = label_table)
    name <- paste("projection", hemi, scale, sep = "-")
    emit(name, result)
    write.csv(label_table, file.path(out, paste0(name, "-labels.csv")),
      row.names = FALSE)
    cases[[name]] <- list(from = "MNI152NLin6Asym", to = "fsLR", hemi = hemi,
      scale = scale, data_type = "projection", input_sha256 = sha(path),
      projection_id = projection$id)
    cat(name, "exported\n")
  }
}
frames <- c("MNI152NLin6Asym", "MNI152NLin2009cAsym")
for (from in frames) {
  to <- setdiff(frames, from)
  operator <- get_template_transform(from, to, cache_dir = cache, offline = TRUE)
  target <- neuroim2::read_vol(file.path(work, "inputs",
    paste0("tpl-", to, "_res-02_desc-brain_T1w.nii.gz")))
  for (suffix in c("desc-brain_T1w", "atlas-HOCPA_desc-th25_dseg")) {
    path <- file.path(work, "inputs", paste0("tpl-", from, "_res-02_", suffix, ".nii.gz"))
    type <- if (suffix == "desc-brain_T1w") "continuous" else "label"
    result <- apply_template_transform(neuroim2::read_vol(path), operator,
      target, data_type = type)
    name <- paste(from, to, suffix, sep = "-")
    neuroim2::write_vol(result, file.path(out, paste0(name, ".nii.gz")))
    cat(name, "exported\n")
  }
}
receipt <- list(protocol_sha256 = sha("data-raw/transform-quality-v1/protocol.md"),
  driver_sha256 = sha("data-raw/transform-quality-v1/public-api.R"),
  source_commit = system("git rev-parse HEAD", intern = TRUE),
  R = R.version.string, cases = cases,
  packages = lapply(c("neuroatlas", "neurotransform", "neuroim2"), function(p) {
    list(package = p, version = as.character(utils::packageVersion(p)),
      sha = utils::packageDescription(p)$RemoteSha)
  }),
  files = as.list(setNames(vapply(list.files(out, full.names = TRUE), sha,
    character(1)), list.files(out))))
jsonlite::write_json(receipt, file.path(out, "receipt.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 17)
