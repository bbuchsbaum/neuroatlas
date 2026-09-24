# Freeze numerical probes independently of any candidate transformation.
entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- dirname(normalizePath(entry, mustWork = TRUE))
source(file.path(root, "scripts", "common.R"))
inputs <- read_json(file.path(work_root(), "qualification-inputs.json"))
out <- Sys.getenv("NEUROATLAS_LANDMARK_OUTPUT", file.path(root, "landmarks-v1"))
if (dir.exists(out)) stop("Refusing to overwrite frozen landmark files.")
dir.create(out)
lookup <- templateflow::tf_get(template = "MNI152NLin6Asym", atlas = "HOSPA",
  suffix = "dseg", extension = ".tsv")
labels <- utils::read.delim(lookup)
sub_ids <- labels$index[grepl("Thalamus|Caudate|Putamen|Pallidum|Brainstem|Hippocampus|Amygdala|Accumbens", labels$name)]
file.copy(lookup, file.path(out, basename(lookup)))
# Deterministic spatial coverage: maximin points from a fixed <=4096-voxel pool.
pick <- function(index, xyz, n = 32L) {
  if (length(index) < n) stop("Too few points in a required stratum.")
  pool <- index[unique(round(seq(1, length(index), length.out = min(4096L, length(index)))))]
  coordinates <- xyz[pool, , drop = FALSE]
  chosen <- 1L
  distance <- rep(Inf, length(pool))
  for (i in seq_len(n - 1L)) {
    distance <- pmin(distance, rowSums(sweep(coordinates, 2L, coordinates[tail(chosen, 1L), ], "-")^2))
    distance[chosen] <- -Inf
    chosen <- c(chosen, which.max(distance))
  }
  pool[chosen]
}
manifest <- list(schema_version = 1, role = "numerical_physical_coordinate_probes",
  interpretation = "Coverage strata only; not manually paired anatomical truth. HOSPA shares the NLin6 label index table in both templates.",
  selection = "32 deterministic maximin samples per hemisphere per disjoint stratum from at most 4096 uniformly indexed candidates; precedence near_edge, boundary, subcortical, cortical, central; x=0 excluded.",
  definitions = list(central = "remaining brain-mask voxels in the nearest quartile to the mask physical centroid",
    cortical = "brain-mask voxels with nonzero HOCPA th25 label",
    subcortical = "brain-mask voxels labelled thalamus, caudate, putamen, pallidum, brainstem, hippocampus, amygdala or accumbens by HOSPA th25",
    boundary = "remaining brain-mask voxels within 2 mm of mask exterior",
    near_edge = "brain-mask voxels within 5 mm of any physical bounding-box face of the brain mask; this is mask extent, not displacement-grid extrapolation"),
  hospa_label_table = file_receipt(lookup), cells = list())
for (side in c("source", "target")) for (res in 1:2) {
  key <- paste(side, res, sep = "_")
  mask_spec <- inputs$files[[paste0(key, "_mask")]]
  mask_path <- file.path(work_root(), "inputs", mask_spec$relpath)
  cortical_path <- file.path(work_root(), "inputs", inputs$files[[paste0(key, "_qa_labels")]]$relpath)
  sub_path <- templateflow::tf_get(template = mask_spec$template, resolution = res,
    atlas = "HOSPA", desc = "th25", suffix = "dseg", extension = ".nii.gz")
  image <- RNifti::readNifti(mask_path)
  mask <- as.array(image) > 0
  cortex <- as.array(RNifti::readNifti(cortical_path))
  sub <- as.array(RNifti::readNifti(sub_path))
  stopifnot(identical(dim(mask), dim(cortex)), identical(dim(mask), dim(sub)))
  index <- which(mask)
  ijk <- arrayInd(index, dim(mask)) - 1L
  xyz <- (cbind(ijk, 1) %*% t(RNifti::xform(image)))[, 1:3]
  interior <- mask
  d <- dim(mask)
  offsets <- expand.grid(dx = -2:2, dy = -2:2, dz = -2:2)
  offsets <- offsets[rowSums((as.matrix(offsets) * res)^2) <= 4, ]
  for (row in seq_len(nrow(offsets))) {
    delta <- as.integer(offsets[row, ])
    low <- high <- vector("list", 3)
    for (axis in 1:3) {
      low[[axis]] <- seq.int(max(1L, 1L - delta[axis]), min(d[axis], d[axis] - delta[axis]))
      high[[axis]] <- low[[axis]] + delta[axis]
    }
    shifted <- array(FALSE, d)
    shifted <- do.call("[<-", c(list(shifted), low, list(value = do.call("[", c(list(mask), high, list(drop = FALSE))))))
    interior <- interior & shifted
  }
  extent_low <- apply(xyz, 2, min); extent_high <- apply(xyz, 2, max)
  near_edge <- apply(sweep(xyz, 2, extent_low, "-") <= 5 | sweep(xyz, 2, extent_high, "-") >= -5, 1, any)
  used <- near_edge
  strata <- list(near_edge = which(used))
  boundary <- !interior[index] & !used
  strata$boundary <- which(boundary); used <- used | boundary
  subcortical <- sub[index] %in% sub_ids & !used
  strata$subcortical <- which(subcortical); used <- used | subcortical
  cortical <- cortex[index] > 0 & !used
  strata$cortical <- which(cortical); used <- used | cortical
  distance <- rowSums(sweep(xyz, 2, colMeans(xyz), "-")^2)
  strata$central <- which(!used & distance <= quantile(distance[!used], .25))
  rows <- do.call(rbind, lapply(names(strata), function(stratum) {
    chosen <- unlist(lapply(c(-1, 1), function(hemisphere) {
      population <- strata[[stratum]]
      population <- population[sign(xyz[population, 1]) == hemisphere]
      pick(population, xyz)
    }))
    data.frame(stratum = stratum, hemisphere = ifelse(xyz[chosen, 1] < 0, "L", "R"),
      i = ijk[chosen, 1], j = ijk[chosen, 2], k = ijk[chosen, 3],
      x = xyz[chosen, 1], y = xyz[chosen, 2], z = xyz[chosen, 3],
      hospa = sub[index[chosen]], hocpa = cortex[index[chosen]])
  }))
  stopifnot(nrow(rows) == 320L, !anyDuplicated(rows[c("i", "j", "k")]))
  path <- file.path(out, paste0(key, ".csv"))
  utils::write.csv(rows, path, row.names = FALSE)
  manifest$cells[[key]] <- list(file = basename(path), receipt = file_receipt(path),
    mask = c(list(path = mask_path), mask_spec[c("bytes", "sha256")]),
    cortical = file_receipt(cortical_path), subcortical = file_receipt(sub_path),
    counts = as.list(table(rows$stratum)), hemispheres = as.list(table(sign(rows$x))))
}
write_json(manifest, file.path(out, "manifest.json"))
message("Frozen landmark files: ", out)
