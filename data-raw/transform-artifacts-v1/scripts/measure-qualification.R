# Candidate-agnostic numerical measurement helpers.  This file has no CLI.

measure_artifact_root <- function() {
  configured <- Sys.getenv("NEUROATLAS_TRANSFORM_ARTIFACT_ROOT", unset = "")
  if (nzchar(configured)) return(normalizePath(configured, mustWork = TRUE))
  here <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(here) == 1L) return(dirname(normalizePath(here, mustWork = TRUE)))
  normalizePath(file.path(getwd(), "data-raw", "transform-artifacts-v1"), mustWork = TRUE)
}

source(file.path(measure_artifact_root(), "scripts", "common.R"))

measure_receipt_path <- function(receipt, root = work_root()) {
  file.path(root, "inputs", receipt$relpath)
}

measure_dice <- function(fixed, moved, source_ids) {
  fixed_ids <- sort(unique(as.integer(source_ids[source_ids != 0])))
  moved_ids <- sort(unique(as.integer(moved[is.finite(moved) & moved != 0])))
  dice <- vapply(fixed_ids, function(id) {
    a <- fixed == id; b <- moved == id
    denominator <- sum(a) + sum(b)
    if (denominator == 0) 0 else 2 * sum(a & b) / denominator
  }, numeric(1))
  names(dice) <- as.character(fixed_ids)
  list(min = if (length(dice)) min(dice) else NA_real_, source_ids = fixed_ids,
       missing_from_warp = setdiff(fixed_ids, moved_ids),
       unexpected_in_warp = setdiff(moved_ids, fixed_ids), per_label = as.list(dice))
}

measure_grid_points <- function(mask_path, n = 256L) {
  mask <- RNifti::readNifti(mask_path)
  index <- which(as.array(mask) > 0)
  if (!length(index)) stop("Frozen mask contains no supported points.", call. = FALSE)
  # A deterministic, mask-stratified subset in voxel order.  Coordinates are
  # converted to RAS by the NIfTI affine below and are numerical probes only.
  picked <- index[unique(round(seq(1, length(index), length.out = min(n, length(index)))))]
  ijk <- arrayInd(picked, dim(mask)) - 1
  affine <- RNifti::xform(mask)
  ras <- cbind(ijk, 1) %*% t(affine)
  ras[, 1:3, drop = FALSE]
}

measure_transform_points <- function(path, points_ras, identity_domain = NULL) {
  transform <- if (is.null(identity_domain)) {
    neurotransform::read_transform(path, type = "ants_h5")
  } else neurotransform::IdentityMorphism(identity_domain)
  as.matrix(neurotransform::transform(transform, points_ras))
}

measure_ants_points <- function(points_ras, transform, output, route) {
  input <- sub("-output\\.csv$", "-input.csv", output)
  lps <- points_ras
  lps[, 1:2] <- -lps[, 1:2, drop = FALSE]
  utils::write.csv(data.frame(x = lps[, 1], y = lps[, 2], z = lps[, 3]), input, row.names = FALSE)
  # Correct the pinned spec locally: processx needs two argv tokens, not a
  # shell-quoted string in the generic `args` input.
  call <- niflowr::ni_call("ants.apply_transforms_to_points", dimension = 3,
    input_file = input, output_file = output, transforms = transform,
    .engine = route$registration$engine, .profile = route$registration$profile,
    .validate = FALSE)
  call$spec$inputs$transforms$cli$argstr <- "-t %s"
  call$spec$inputs$transforms$items_type <- "file"
  command <- niflowr::ni_cmd(call)$args
  flag <- match("-t", command)
  if (is.na(flag) || flag == length(command) || command[[flag + 1L]] != transform) {
    stop("ANTs points command lacks a separate transform argument.", call. = FALSE)
  }
  niflowr::ni_run(call, timeout = route$registration$timeout_seconds, echo = FALSE)
  result <- as.matrix(utils::read.csv(output)[, c("x", "y", "z")])
  result[, 1:2] <- -result[, 1:2, drop = FALSE]
  result
}

measure_warp <- function(input, reference, transform, interpolation, output,
                         route) {
  call <- niflowr::ni_call("ants.apply_transforms", dimension = 3,
    input_image = input, reference_image = reference, transforms = transform,
    interpolation = interpolation, output_image = output,
    .engine = route$registration$engine, .profile = route$registration$profile)
  niflowr::ni_run(call, timeout = route$registration$timeout_seconds,
                  echo = FALSE)
  if (!file.exists(output) || file.info(output)$size < 1L) stop("ANTs did not write warped output.", call. = FALSE)
  output
}

measure_resampling_plan <- function(source_path, target_path, transform, interpolation,
                                    identity_domain = NULL) {
  source <- RNifti::readNifti(source_path); target <- RNifti::readNifti(target_path)
  source_grid <- neurotransform::grid_spec(dim(source)[1:3], RNifti::xform(source))
  target_grid <- neurotransform::grid_spec(dim(target)[1:3], RNifti::xform(target))
  morphology <- if (is.null(identity_domain)) neurotransform::read_transform(transform, type = "ants_h5") else neurotransform::IdentityMorphism(identity_domain)
  neurotransform::make_resampling_plan(morphology, source_grid, target_grid,
    interpolation = interpolation, reuse_count = 2L, cache = FALSE)
}

measure_resampling_oracle <- function(plan, values) {
  neurotransform::apply_resampling_plan(plan, values, outside = 0, modulate = "none")
}

measure_geometry_exact <- function(path, reference) {
  image <- RNifti::readNifti(path); target <- RNifti::readNifti(reference)
  identical(as.integer(dim(image)[1:3]), as.integer(dim(target)[1:3])) &&
    identical(as.numeric(RNifti::xform(image)), as.numeric(RNifti::xform(target)))
}

measure_image_roundtrip <- function(original_mask, original_labels,
                                    returned_mask, returned_labels,
                                    reference_image) {
  geometry_exact <- measure_geometry_exact(returned_mask, reference_image) &&
    measure_geometry_exact(returned_labels, reference_image)
  mask <- as.array(RNifti::readNifti(original_mask)) > 0
  raw_mask_back <- as.array(RNifti::readNifti(returned_mask))
  labels <- as.array(RNifti::readNifti(original_labels))
  labels_back <- as.array(RNifti::readNifti(returned_labels))
  ids <- sort(unique(as.integer(labels[is.finite(labels) & labels != 0])))
  nonfinite_mask <- sum(!is.finite(raw_mask_back))
  nonfinite_labels <- sum(!is.finite(labels_back))
  # Invalid/mismatched data make Dice unavailable; never drop bad voxels.
  dice <- list(min = NA_real_, source_ids = ids,
    missing_from_warp = NA_integer_, unexpected_in_warp = NA_integer_,
    per_label = setNames(as.list(rep(NA_real_, length(ids))), as.character(ids)))
  mask_dice <- NA_real_
  integer_labels <- !nonfinite_labels && all(labels_back == round(labels_back))
  if (geometry_exact && integer_labels) {
    dice <- measure_dice(labels, labels_back, ids)
  }
  if (geometry_exact && !nonfinite_mask) {
    mask_back <- raw_mask_back > 0
    denominator <- sum(mask) + sum(mask_back)
    mask_dice <- if (denominator == 0) 0 else {
      2 * sum(mask & mask_back) / denominator
    }
  }
  list(mask_dice = mask_dice, label_dice = dice,
    geometry_exact = geometry_exact,
    finite = !nonfinite_mask && !nonfinite_labels,
    integer_labels = integer_labels,
    allowed_label_ids = !nonfinite_labels && all(labels_back %in% c(0, ids)),
    lost_ids = dice$missing_from_warp, nonfinite_mask = nonfinite_mask,
    nonfinite_labels = nonfinite_labels)
}

measure_h5_warp_grid <- function(path) {
  h5 <- hdf5r::H5File$new(path, mode = "r")
  on.exit(h5$close_all(), add = TRUE)
  groups <- h5[["TransformGroup"]]$ls(recursive = FALSE)$name
  for (group in groups) {
    node <- h5[[paste0("TransformGroup/", group)]]
    type <- as.character(node[["TransformType"]]$read())
    if (any(grepl("DisplacementFieldTransform", type, fixed = TRUE))) {
      key <- intersect(c("TransformFixedParameters", "TranformFixedParameters"),
                       node$ls(recursive = FALSE)$name)
      if (length(key) != 1L) stop("Missing or ambiguous H5 fixed parameters.", call. = FALSE)
      fixed <- as.numeric(node[[key]]$read())
      if (length(fixed) != 18L) stop("ANTs H5 displacement fixed parameters are invalid.", call. = FALSE)
      direction <- matrix(fixed[10:18], nrow = 3L, byrow = TRUE)
      vox_to_lps <- diag(4); vox_to_lps[1:3, 1:3] <- direction %*% diag(fixed[7:9]); vox_to_lps[1:3, 4] <- fixed[4:6]
      lps_to_ras <- diag(c(-1, -1, 1, 1))
      return(list(dim = as.integer(fixed[1:3]), world_to_vox = solve(lps_to_ras %*% vox_to_lps)))
    }
  }
  NULL
}

measure_warp_support <- function(path, points_ras, identity = FALSE) {
  if (isTRUE(identity)) return(list(supported = rep(TRUE, nrow(points_ras)), boundary = rep(FALSE, nrow(points_ras)), available = TRUE))
  grid <- measure_h5_warp_grid(path)
  if (is.null(grid)) return(list(supported = rep(TRUE, nrow(points_ras)), boundary = rep(FALSE, nrow(points_ras)), available = TRUE))
  morphism <- neurotransform::read_transform(path, type = "ants_h5")
  components <- if (methods::is(morphism, "MorphismPath")) rev(morphism@morphisms) else list(morphism)
  at_warp <- which(vapply(components, function(x) methods::is(x, "Warp3DMorphism"), logical(1)))
  coords <- points_ras
  if (length(at_warp) && at_warp[[1L]] > 1L) {
    for (component in components[seq_len(at_warp[[1L]] - 1L)]) coords <- neurotransform::transform(component, coords)
  }
  vox <- cbind(coords, 1) %*% t(grid$world_to_vox)
  lower <- rep(0, 3L); upper <- grid$dim - 1L
  supported <- apply(vox[, 1:3, drop = FALSE] >= rep(lower, each = nrow(vox)) & vox[, 1:3, drop = FALSE] <= rep(upper, each = nrow(vox)), 1L, all)
  boundary <- supported & apply(abs(vox[, 1:3, drop = FALSE]) < 1e-8 |
    abs(vox[, 1:3, drop = FALSE] - rep(upper, each = nrow(vox))) < 1e-8, 1L, any)
  list(supported = supported, boundary = boundary, available = TRUE, voxel = vox[, 1:3, drop = FALSE])
}

measure_qualification_cell <- function(direction, resolution, inputs, forward,
                                       inverse, out_dir, route,
                                       repeat_forward = NULL,
                                       repeat_inverse = NULL, identity = FALSE,
                                       landmarks = NULL) {
  stopifnot(direction %in% c("forward", "inverse"), resolution %in% c(1L, 2L))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  moving_side <- if (direction == "forward") "source" else "target"
  fixed_side <- if (direction == "forward") "target" else "source"
  transform <- if (direction == "forward") forward else inverse
  paired <- if (direction == "forward") inverse else forward
  path <- function(side, kind, res = resolution) measure_receipt_path(inputs$files[[paste(side, res, kind, sep = "_")]])
  moving_image <- path(moving_side, "image", 1L)
  moving_mask <- path(moving_side, "mask", 1L)
  moving_labels <- path(moving_side, "qa_labels", 1L)
  fixed_image <- path(fixed_side, "image")
  fixed_mask <- path(fixed_side, "mask")
  fixed_labels <- path(fixed_side, "qa_labels")
  qa <- niflowr::ni_ants_registration_qa(fixed_image = fixed_image,
    moving_image = moving_image, transform = transform, fixed_mask = fixed_mask,
    moving_mask = moving_mask, fixed_labels = fixed_labels,
    moving_labels = moving_labels, out_dir = file.path(out_dir, "ants-qa"),
    .engine = route$registration$engine, .profile = route$registration$profile,
    timeout = route$registration$timeout_seconds, echo = FALSE)
  nearest_labels <- measure_warp(moving_labels, fixed_image, transform,
    "NearestNeighbor", file.path(out_dir, "labels-nearest.nii.gz"), route)
  mask_roundtrip <- measure_warp(qa$files$warped_mask, moving_image, paired,
    "NearestNeighbor", file.path(out_dir, "mask-roundtrip.nii.gz"), route)
  labels_roundtrip <- measure_warp(nearest_labels, moving_image, paired,
    "NearestNeighbor", file.path(out_dir, "labels-roundtrip.nii.gz"), route)
  image_roundtrip <- measure_image_roundtrip(moving_mask, moving_labels,
    mask_roundtrip, labels_roundtrip, moving_image)
  source_label_values <- as.array(RNifti::readNifti(moving_labels))
  storage.mode(source_label_values) <- "double"
  labels <- measure_dice(as.array(RNifti::readNifti(fixed_labels)),
    as.array(RNifti::readNifti(nearest_labels)), unique(as.integer(source_label_values)))
  source_values <- as.array(RNifti::readNifti(moving_image))
  finite_source <- source_values[is.finite(source_values)]
  scalar <- (source_values - min(finite_source)) / diff(range(finite_source))
  scalar[!is.finite(scalar)] <- 0
  scalar_source <- file.path(out_dir, "scalar-source.nii.gz")
  RNifti::writeNifti(scalar, scalar_source, template = moving_image)
  scalar_ants_path <- measure_warp(scalar_source, fixed_image, transform, "Linear",
    file.path(out_dir, "scalar-linear.nii.gz"), route)
  scalar_ants <- as.array(RNifti::readNifti(scalar_ants_path))
  identity_domain <- if (isTRUE(identity)) paste(moving_side, "to", fixed_side) else NULL
  linear_plan <- measure_resampling_plan(moving_image, fixed_image, transform, "linear", identity_domain)
  nearest_plan <- measure_resampling_plan(moving_image, fixed_image, transform, "nearest", identity_domain)
  scalar_oracle <- measure_resampling_oracle(linear_plan, scalar)
  probability <- array(0, dim = c(dim(scalar), 2L))
  probability[, , , 1L] <- 0.6 * scalar
  probability[, , , 2L] <- 0.6 - probability[, , , 1L]
  probability_paths <- vapply(1:2, function(channel) {
    source <- file.path(out_dir, paste0("probability-", channel, "-source.nii.gz"))
    output <- file.path(out_dir, paste0("probability-", channel, "-linear.nii.gz"))
    RNifti::writeNifti(probability[, , , channel], source, template = moving_image)
    measure_warp(source, fixed_image, transform, "Linear", output, route)
  }, character(1))
  probability_ants <- lapply(probability_paths, function(path) as.array(RNifti::readNifti(path)))
  probability_4d <- measure_resampling_oracle(linear_plan, probability)
  probability_oracle <- lapply(1:2, function(channel) probability_4d[, , , channel])
  label_oracle <- measure_resampling_oracle(nearest_plan, source_label_values)
  source_dim <- dim(source_values)[1:3]
  source_axes <- RNifti::xform(RNifti::readNifti(moving_image))[1:3, 1:3]
  source_spacing <- sqrt(colSums(source_axes^2))
  unit_axes <- sweep(source_axes, 2L, source_spacing, "/")
  if (max(abs(crossprod(unit_axes) - diag(3))) > 1e-8) {
    stop("Physical-gradient budget requires orthogonal source voxel axes.")
  }
  source_coords <- linear_plan$coords_src_vox
  interior <- apply(source_coords > 0 & source_coords < rep(source_dim - 1L, each = nrow(source_coords)), 1L, all)
  scalar_error <- abs(as.vector(scalar_ants) - as.vector(scalar_oracle))
  probability_error <- vapply(seq_along(probability_ants), function(i) max(abs(as.vector(probability_ants[[i]]) - as.vector(probability_oracle[[i]]))[interior], na.rm = TRUE), numeric(1))
  label_mismatch <- as.vector(RNifti::readNifti(nearest_labels)) != as.vector(label_oracle)
  tie_tolerance <- 1e-3 / source_spacing
  tie_or_edge <- apply(abs(source_coords - (floor(source_coords) + 0.5)) <= rep(tie_tolerance, each = nrow(source_coords)) | source_coords <= rep(tie_tolerance, each = nrow(source_coords)) | source_coords >= rep(source_dim - 1L - tie_tolerance, each = nrow(source_coords)), 1L, any)
  allowed_difference_path <- file.path(out_dir,
    "label-oracle-allowed-differences.nii.gz")
  RNifti::writeNifti(array(tie_or_edge, dim(scalar_ants)),
    allowed_difference_path, template = fixed_image)
  reference_outputs <- lapply(list(scalar = scalar_ants_path,
    labels = nearest_labels, allowed_label_differences = allowed_difference_path),
    file_receipt)
  axis_gradient <- c(max(abs(scalar[-1, , ] - scalar[-dim(scalar)[1L], , ]), na.rm = TRUE), max(abs(scalar[, -1, ] - scalar[, -dim(scalar)[2L], ]), na.rm = TRUE), max(abs(scalar[, , -1] - scalar[, , -dim(scalar)[3L]]), na.rm = TRUE)) / source_spacing
  scalar_gradient <- sqrt(sum(axis_gradient^2))
  jac <- as.array(RNifti::readNifti(qa$files$jacobian))
  jacobian_region <- as.array(RNifti::readNifti(fixed_mask)) > 0
  jacobian_values <- jac[jacobian_region]
  points <- if (is.null(landmarks)) measure_grid_points(fixed_mask) else
    as.matrix(landmarks[, c("x", "y", "z")])
  strata <- if (is.null(landmarks)) rep("legacy_uniform", nrow(points)) else landmarks$stratum
  mapped <- measure_transform_points(transform, points, identity_domain)
  ants_mapped <- measure_ants_points(points, transform, file.path(out_dir, "points-output.csv"), route)
  official_transform <- measure_receipt_path(inputs$files[[paste0("official_", direction)]])
  official_mapped <- measure_transform_points(official_transform, points)
  returned <- measure_transform_points(paired, mapped, identity_domain)
  forward_support <- measure_warp_support(transform, points, identity)
  inverse_support <- measure_warp_support(paired, mapped, identity)
  support <- is.finite(mapped[, 1]) & is.finite(mapped[, 2]) & is.finite(mapped[, 3]) & forward_support$supported & inverse_support$supported
  official_support <- support & apply(is.finite(official_mapped), 1L, all)
  point_error <- if (any(support)) max(sqrt(rowSums((mapped[support, , drop = FALSE] - ants_mapped[support, , drop = FALSE])^2))) else Inf
  roundtrip_error <- if (any(support)) max(sqrt(rowSums((returned[support, , drop = FALSE] - points[support, , drop = FALSE])^2))) else Inf
  repeat_error <- NA_real_
  repeat_support_complete <- is.null(repeat_forward) && is.null(repeat_inverse)
  repeat_transform <- if (direction == "forward") repeat_forward else repeat_inverse
  if (!is.null(repeat_transform)) {
    repeated <- measure_transform_points(repeat_transform, points)
    repeat_support <- measure_warp_support(repeat_transform, points)
    both <- support & apply(is.finite(repeated), 1L, all) & repeat_support$supported
    repeat_support_complete <- all(both)
    repeat_error <- if (any(both)) max(sqrt(rowSums((mapped[both, , drop = FALSE] - repeated[both, , drop = FALSE])^2))) else Inf
  }
  point_summary <- function(error) {
    bad <- sum(!is.finite(error))
    if (bad) return(list(median = NA_real_, p95 = NA_real_, max = NA_real_, nonfinite = bad))
    list(median = median(error), p95 = unname(quantile(error, .95)),
      max = max(error), nonfinite = 0L)
  }
  roundtrip_distances <- sqrt(rowSums((returned - points)^2))
  roundtrip_summary <- c(list(all = point_summary(roundtrip_distances)),
    lapply(split(roundtrip_distances, strata), point_summary))
  warped_label_values <- as.array(RNifti::readNifti(nearest_labels))
  geometry_paths <- unique(c(nearest_labels, scalar_ants_path, probability_paths,
    unlist(qa$files[c("warped_image", "warped_mask", "warped_labels", "jacobian")])))
  similarity <- setNames(qa$similarity$value, qa$similarity$metric)
  metrics <- list(mi_cost = unname(similarity[["MI"]]), cc_cost = unname(similarity[["CC"]]),
    mask_dice = unname(qa$mask_dice), label_dice_min = labels$min,
    point_error_max_mm = point_error, roundtrip_error_max_mm = roundtrip_error,
    repeat_point_error_max_mm = repeat_error,
    scalar_error_max = max(scalar_error[interior], na.rm = TRUE),
    probability_min = min(unlist(probability_ants), na.rm = TRUE),
    probability_max = max(unlist(probability_ants), na.rm = TRUE), jacobian_min = min(jacobian_values, na.rm = TRUE),
    nonpositive_jacobians = sum(jacobian_values <= 0, na.rm = TRUE), nonfinite_jacobians = sum(!is.finite(jacobian_values)))
  result <- list(
    direction = direction, resolution = as.integer(resolution), metrics = metrics,
    reference_outputs = reference_outputs,
    invariants = list(
      target_geometry_exact = all(vapply(geometry_paths, measure_geometry_exact, logical(1), reference = fixed_image)),
      label_ids_preserved = all(is.finite(warped_label_values)) && all(warped_label_values == round(warped_label_values)) && !length(labels$unexpected_in_warp) && all(label_oracle %in% c(0, unique(as.vector(source_label_values)))),
      label_oracle_agreement = !any(label_mismatch & !tie_or_edge),
      finite_outputs = all(is.finite(jacobian_values)) && all(is.finite(scalar_ants)) && all(vapply(probability_ants, function(x) all(is.finite(x)), logical(1))) && all(is.finite(scalar_oracle)) && all(is.finite(probability_4d)) && all(is.finite(label_oracle)),
      point_support_complete = all(support) && all(is.finite(returned)),
      repeat_support_complete = repeat_support_complete,
      landmark_coverage_complete = !is.null(landmarks) &&
        identical(sort(names(table(strata))), sort(c("central", "cortical", "subcortical", "boundary", "near_edge"))) &&
        all(table(strata) == 64L) && !anyDuplicated(landmarks[c("i", "j", "k")]),
      independent_reader_pass = if (isTRUE(identity)) !is.null(neurotransform::read_linear_transform(transform, format = "itk")) else !is.null(neurotransform::read_transform(transform, type = "ants_h5")),
      probability_channels_preserved = all(vapply(c(probability_ants, probability_oracle), function(x) all(is.finite(x) & x >= -1e-6 & x <= 1 + 1e-6), logical(1))) && all(probability_error <= scalar_gradient * 1e-3 + 1e-6) && all(abs(probability_oracle[[1L]][interior] + probability_oracle[[2L]][interior] - 0.6) <= 1e-6)
    ),
    raw = list(
      label_dice = labels, image_roundtrip = image_roundtrip, roundtrip_mm = roundtrip_summary, point_count = nrow(points), unsupported_points = sum(!support), boundary_points = sum(forward_support$boundary | inverse_support$boundary), qa_similarity = qa$similarity,
      candidate_vs_official_point_error_max_mm = if (any(official_support)) max(sqrt(rowSums((mapped[official_support, , drop = FALSE] - official_mapped[official_support, , drop = FALSE])^2))) else Inf,
      scalar_tolerance = scalar_gradient * 1e-3 + 1e-6,
      probability_oracle_range = range(unlist(probability_oracle)), scalar_error_all_grid_max = max(scalar_error, na.rm = TRUE), scalar_error_boundary_max = max(scalar_error[!interior], na.rm = TRUE), label_disagreements = sum(label_mismatch), label_allowed_tie_or_edge = sum(label_mismatch & tie_or_edge), probability_oracle_error_max = max(abs(unlist(probability_ants) - unlist(probability_oracle)), na.rm = TRUE)
    ),
    paths = c(qa$files, list(nearest_labels = nearest_labels, scalar = scalar_ants_path, probability = probability_paths, mask_roundtrip = mask_roundtrip, labels_roundtrip = labels_roundtrip))
  )
  result
}
