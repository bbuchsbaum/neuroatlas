#!/usr/bin/env Rscript
# Inspect locked GIFTI assets and write a small, explicit environment smoke set.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
input <- normalizePath(args[[1]], mustWork = TRUE)
output <- args[[2]]
stopifnot(!dir.exists(output))
dir.create(output, recursive = TRUE)
lock_path <- file.path(dirname(input), "inputs.lock.json")
if (!file.exists(lock_path)) {
  lock_path <- "data-raw/surface-transforms-v1/inputs.lock.json"
}
lock <- jsonlite::read_json(lock_path, simplifyVector = FALSE)
sha <- function(path) digest::digest(file = path, algo = "sha256")
for (a in lock$assets) {
  path <- file.path(input, a$path)
  stopifnot(file.info(path)$size == a$size_bytes, sha(path) == a$sha256)
}
asset <- function(template, hemi, role) {
  selected <- Filter(function(a) identical(a$template, template) &&
    identical(a$hemisphere, hemi) && identical(a$role, role), lock$assets)
  stopifnot(length(selected) == 1L)
  selected[[1]]$path
}
read_asset <- function(name) gifti::readgii(file.path(input, name))
array_sha <- function(x, integer = FALSE) {
  # Canonical row-major little-endian bytes; dimensions recorded separately.
  value <- if (is.matrix(x)) as.vector(t(x)) else as.vector(x)
  raw <- writeBin(if (integer) as.integer(value) else as.double(value),
    raw(), size = if (integer) 4L else 8L, endian = "little")
  digest::digest(raw, algo = "sha256", serialize = FALSE)
}
write_gifti <- function(values, path, hemi, labels = FALSE) {
  table <- if (labels) paste0('<LabelTable>', paste(vapply(
    sort(unique(c(0L, values))), function(key) {
      if (key == 0L) {
        # Workbench resolves the unassigned key by the name "???".
        return('<Label Key="0" Red="0" Green="0" Blue="0" Alpha="0">???</Label>')
      }
      sprintf(paste0('<Label Key="%d" Red="0.5" Green="0.5" ',
        'Blue="0.5" Alpha="1">label_%d</Label>'), key, key)
    }, character(1)), collapse = ""), '</LabelTable>') else
    '<LabelTable/>'
  text <- paste0('<?xml version="1.0" encoding="UTF-8"?>',
    '<GIFTI Version="1.0" NumberOfDataArrays="1">',
    '<MetaData><MD><Name>AnatomicalStructurePrimary</Name><Value>Cortex',
    if (hemi == "L") "Left" else "Right", '</Value></MD></MetaData>',
    table, '<DataArray Intent="',
    if (labels) 'NIFTI_INTENT_LABEL' else 'NIFTI_INTENT_SHAPE',
    '" DataType="', if (labels) 'NIFTI_TYPE_INT32' else 'NIFTI_TYPE_FLOAT32',
    '" ArrayIndexingOrder="RowMajorOrder" Dimensionality="1" Dim0="',
    length(values), '" Encoding="ASCII" Endian="LittleEndian"',
    ' ExternalFileName="" ExternalFileOffset="0"><MetaData/><Data>',
    paste(values, collapse = " "), '</Data></DataArray></GIFTI>')
  writeLines(text, path, useBytes = TRUE)
  stopifnot(identical(as.numeric(gifti::readgii(path)$data[[1]]),
    as.numeric(values)))
}
domains <- list()
for (template in c("fsaverage", "fsLR")) for (hemi in c("L", "R")) {
  density <- if (template == "fsaverage") "164k" else "32k"
  n <- if (template == "fsaverage") 163842L else 32492L
  sphere_path <- asset(template, hemi,
    if (template == "fsaverage") "sphere" else "registered_sphere")
  sphere <- read_asset(sphere_path)
  points <- sphere$data$pointset
  faces <- sphere$data$triangle
  stopifnot(identical(dim(points), c(n, 3L)), all(is.finite(points)),
    ncol(faces) == 3L, nrow(faces) == 2L * n - 4L,
    all(is.finite(faces)), all(faces == trunc(faces)),
    min(faces) == 0L, max(faces) == n - 1L,
    length(unique(as.vector(faces))) == n,
    all(faces[, 1] != faces[, 2]), all(faces[, 1] != faces[, 3]),
    all(faces[, 2] != faces[, 3]))
  anatomical <- read_asset(asset(template, hemi, "midthickness"))
  stopifnot(identical(anatomical$data$triangle, faces),
    identical(dim(anatomical$data$pointset), c(n, 3L)))
  if (template == "fsaverage") {
    reference <- read_asset(asset(template, hemi, "ordering_reference"))
    stopifnot(identical(reference$data, sphere$data))
  } else {
    native <- read_asset(asset(template, hemi, "sphere"))
    stopifnot(identical(native$data$triangle, faces))
  }
  area_path <- asset(template, hemi, "area")
  area <- as.numeric(read_asset(area_path)$data[[1]])
  mask_path <- asset(template, hemi, "mask")
  mask <- as.numeric(read_asset(mask_path)$data[[1]])
  stopifnot(length(area) == n, all(is.finite(area)), all(area > 0),
    length(mask) == n, all(mask %in% c(0, 1)), any(mask == 0), any(mask == 1))
  id <- paste(template, density, hemi, sep = "-")
  constant <- if (hemi == "L") 7 else 13
  label_values <- ifelse(points[, 1] >= 0,
    if (hemi == "L") 11L else 21L, if (hemi == "L") 12L else 22L)
  label_values[mask == 0] <- 0L
  for (kind in c("roi", "constant", "labels")) {
    values <- switch(kind, roi = mask, constant = rep(constant, n),
      labels = label_values)
    extension <- if (kind == "labels") ".label.gii" else ".shape.gii"
    write_gifti(values, file.path(output, paste0(id, "-", kind, extension)),
      hemi, labels = kind == "labels")
  }
  domains[[id]] <- list(id = id, template = template, density = density,
    hemisphere = hemi, vertices = n, faces = nrow(faces),
    sphere = sphere_path, correspondence_frame = "fsaverage",
    sphere_sha256 = sha(file.path(input, sphere_path)),
    ordered_faces_sha256_i32le = array_sha(faces, TRUE),
    ordered_vertices_sha256_f64le = array_sha(points),
    area = area_path, mask = mask_path, included_vertices = sum(mask),
    area_sum = sum(area), constant = constant,
    allowed_labels = sort(unique(label_values)),
    qualification = "not_qualified")
}
jsonlite::write_json(list(schema = "neuroatlas.surface-domains.v1",
  input_lock_sha256 = sha(lock_path), domains = domains),
  file.path(output, "domains.json"), auto_unbox = TRUE, pretty = TRUE,
  digits = NA)
writeLines(capture.output(sessionInfo()), file.path(output, "R-session.txt"))
cat("Verified", length(domains), "domains; wrote 12 smoke fixtures.\n")
