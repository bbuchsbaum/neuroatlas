# Export real public-API projections for the frozen supplementary comparison.
# Rscript data-raw/projection-diagnostics-v1/export.R QUALITY_WORK NEW_OUTPUT
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L, !dir.exists(args[[2L]]))
work <- normalizePath(args[[1L]])
out <- args[[2L]]
dir.create(out, recursive = TRUE)
devtools::load_all(quiet = TRUE)
sha <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
implementation_sha <- sha("R/surface_projection.R")
lock <- jsonlite::read_json(paste0(
  "data-raw/transform-quality-v1/evidence-20261007/",
  "cbig-volume-inputs-verified.json"
))
for (entry in lock) {
  stopifnot(identical(sha(file.path(work, "inputs", entry$path)), entry$sha256))
}
emit <- function(name, result) {
  writeBin(as.double(result$values), file.path(out, paste0(name, ".bin")),
    size = 8L, endian = "little")
  writeBin(as.integer(result$coverage$available),
    file.path(out, paste0(name, "-available.bin")), size = 4L, endian = "little")
}
cases <- list()
for (hemi in c("L", "R")) {
  base <- get_surface_geometry("fsaverage", "164k", hemi,
    cache_dir = file.path(work, "cache"), offline = TRUE)
  target <- get_surface_geometry("fsLR", "32k", hemi,
    cache_dir = file.path(work, "cache"), offline = TRUE)
  intermediate_projection <- get_surface_projection("MNI152NLin6Asym", base,
    cache_dir = file.path(work, "cache"), offline = TRUE)
  projection <- get_surface_projection("MNI152NLin6Asym", target,
    cache_dir = file.path(work, "cache"), offline = TRUE)
  for (scale in c(100, 400, 1000)) {
    names <- jsonlite::read_json(file.path(work, "surface-inputs",
      paste0("labels-", scale, ".json")), simplifyVector = TRUE)
    table <- data.frame(key = as.numeric(names(names)), name = as.character(names))
    lut <- utils::read.table(file.path(work, "inputs",
      paste0("Schaefer2018_", scale, "Parcels_7Networks_order.txt")))
    stopifnot(identical(as.character(lut$V2), table$name[match(lut$V1, table$key)]))
    nonzero <- table$key != 0
    stopifnot(all(grepl("^7Networks_[LR]H_", table$name[nonzero])))
    table$hemisphere <- NA_character_
    table$hemisphere[nonzero] <- sub("^7Networks_([LR])H_.*", "\\1",
      table$name[nonzero])
    volume <- neuroim2::read_vol(file.path(work, "inputs", paste0(
      "Schaefer2018_", scale, "Parcels_7Networks_order_FSLMNI152_1mm.nii.gz")))
    sampled <- apply_surface_projection(volume, intermediate_projection,
      source_space = "MNI152NLin6Asym", data_type = "label", label_table = table)
    result <- apply_surface_projection(volume, projection,
      source_space = "MNI152NLin6Asym", data_type = "label", label_table = table)
    baseline_name <- paste("projection", hemi, scale, sep = "-")
    baseline <- readBin(file.path(work, "public-03", paste0(baseline_name, ".bin")),
      "double", n = target$domain$n_vertices, size = 8L, endian = "little")
    availability <- readBin(file.path(work, "public-03",
      paste0(baseline_name, "-available.bin")), "integer",
      n = target$domain$n_vertices, size = 4L, endian = "little")
    stopifnot(identical(as.double(result$values), baseline),
      identical(as.integer(result$coverage$available), availability))
    name <- paste(hemi, scale, sep = "-")
    largest <- apply_surface_transform(sampled, projection$surface_operator,
      label_method = "largest")
    emit(paste0(name, "-sampled"), sampled)
    emit(paste0(name, "-aggregate"), result)
    emit(paste0(name, "-largest"), largest)
    qa <- projection_diagnostics(result)
    utils::write.csv(qa$labels, file.path(out, paste0(name, "-labels.csv")),
      row.names = FALSE)
    utils::write.csv(qa$summary, file.path(out, paste0(name, "-summary.csv")),
      row.names = FALSE)
    cases[[name]] <- list(hemisphere = hemi, parcels = scale,
      projection_id = projection$id,
      operator_id = projection$surface_operator$integrity,
      unchanged_values_and_availability = TRUE)
    cat(name, "exported\n")
  }
}
files <- list.files(out, full.names = TRUE)
stopifnot(identical(implementation_sha, sha("R/surface_projection.R")))
jsonlite::write_json(list(
  protocol_sha256 = sha("data-raw/projection-diagnostics-v1/protocol.md"),
  driver_sha256 = sha("data-raw/projection-diagnostics-v1/export.R"),
  implementation_sha256 = implementation_sha,
  baseline_commit = system("git rev-parse HEAD", intern = TRUE),
  cases = cases,
  engine_build = jsonlite::read_json(Sys.getenv("NEUROATLAS_ENGINE_BINDING"))[
    c("engine_revision", "archive_sha256", "version")],
  R = R.version.string,
  files = as.list(stats::setNames(vapply(files, sha, character(1)), basename(files)))
), file.path(out, "receipt.json"), pretty = TRUE, auto_unbox = TRUE)
