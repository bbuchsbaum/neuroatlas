#' Resolve and Load a Verified Template Transform
#'
#' Resolves an available image-space route and loads its verified artifacts.
#' Planning and fitting are separate: this function never estimates a
#' registration. Nonlinear routes must have a published checksum and a passing
#' qualification record in [space_transform_manifest()].
#'
#' @param from,to Exact source and target template identifiers.
#' @param provider Artifact provider, or `"auto"` for registry selection.
#' @param download Allow missing artifacts to be downloaded.
#' @param verify Must be `TRUE`; artifact integrity cannot be disabled.
#' @param cache_dir Dedicated transform cache directory.
#' @param offline Use only verified local artifacts.
#' @return A `template_transform` containing the plan, files, pullback morphism,
#'   and artifact provenance. Its direction describes image movement; its
#'   morphism maps target coordinates into source coordinates for sampling.
#' @seealso [apply_template_transform()], [transform_atlas()]
#' @export
get_template_transform <- function(from, to,
                                   provider = c("auto", "neuroatlas", "templateflow"),
                                   download = TRUE, verify = TRUE,
                                   cache_dir = transform_cache_path(),
                                   offline = FALSE) {
  provider <- match.arg(provider)
  cache_dir <- .transform_cache_root(cache_dir)
  for (flag in list(download, verify, offline)) {
    assertthat::assert_that(is.logical(flag), length(flag) == 1L, !is.na(flag))
  }
  if (!verify) stop("Transform integrity verification cannot be disabled.")
  plan <- atlas_transform_plan(from, to, data_type = "voxel", mode = "strict",
                               available_only = TRUE, provider = provider)
  .require_neurotransform()
  steps <- plan$steps
  supported <- steps$backend %in% c("identity", "internal_affine")
  if ("format" %in% names(steps)) {
    supported <- supported | (!is.na(steps$format) & steps$format == "ants_h5")
  }
  if (length(supported) != nrow(steps) || !all(supported)) {
    stop("Route has no registered execution backend for volumetric data.")
  }
  files <- rep(NA_character_, nrow(steps))
  morphisms <- vector("list", nrow(steps))
  for (i in seq_len(nrow(steps))) {
    step <- steps[i, , drop = FALSE]
    morphisms[[i]] <- if (step$backend == "identity") {
      neurotransform::IdentityMorphism(step$from_space)
    } else if (step$backend == "internal_affine") {
      # Coordinate transforms are forward maps; image sampling needs pullback.
      matrix <- solve(get_space_transform(step$from_space, step$to_space))
      neurotransform::Affine3DMorphism(step$from_space, step$to_space,
                                       matrix = matrix)
    } else {
      if (!identical(step$convention, "ants_image_pullback_ras")) {
        stop("Artifact lacks a qualified ANTs image pullback convention.")
      }
      loaded <- .fetch_transform_artifact(
        step, cache_dir, download = download, offline = offline, verify = verify,
        .use = function(path) list(path = path,
          morphism = neurotransform::ants_h5_morphism(
            path, source = step$from_space, target = step$to_space))
      )
      files[[i]] <- loaded$path
      loaded$morphism
    }
  }
  morphism <- Reduce(neurotransform::compose, morphisms)
  structure(list(
    from_space = plan$from_space, to_space = plan$to_space,
    plan = plan, files = files, morphism = morphism, cache_dir = cache_dir,
    engine_version = as.character(utils::packageVersion("neurotransform")),
    engine_source_sha = .neurotransform_source_sha(),
    engine_compatibility = "simpleitk-h5-conventions-v1"
  ), class = c("template_transform", "list"))
}

.require_neurotransform <- function() {
  if (!requireNamespace("neurotransform", quietly = TRUE)) {
    stop("Template application requires the optional 'neurotransform' package.")
  }
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Template transforms require the optional 'hdf5r' package. ",
         "Install it with install.packages('hdf5r').")
  }
  required <- c("ants_h5_morphism", "make_resampling_plan",
                "apply_resampling_plan", "grid_spec", "compose")
  if (!all(required %in% getNamespaceExports("neurotransform"))) {
    stop("Installed neurotransform lacks the required template apply API.")
  }
  if (!isTRUE(.neurotransform_compatibility$passed)) {
    .check_neurotransform_semantics()
    .neurotransform_compatibility$passed <- TRUE
  }
}

.neurotransform_compatibility <- new.env(parent = emptyenv())

.neurotransform_source_sha <- function() {
  sha <- utils::packageDescription("neurotransform")$RemoteSha
  if (is.null(sha)) NA_character_ else as.character(sha)
}

.check_neurotransform_semantics <- function() {
  root <- system.file("extdata", "transform-engine-probe", package = "neuroatlas")
  if (!nzchar(root)) stop("Missing bundled transform-engine compatibility probe.")
  expected <- utils::read.csv(file.path(root, "points.csv"))
  compatible <- tryCatch({
    all(vapply(unique(expected$transform), function(name) {
      rows <- expected[expected$transform == name, ]
      morphism <- neurotransform::ants_h5_morphism(file.path(root, name))
      actual <- as.matrix(neurotransform::transform(morphism,
        as.matrix(rows[, c("x", "y", "z")])))
      reference <- as.matrix(rows[, c("expected_x", "expected_y", "expected_z")])
      identical(dim(actual), dim(reference)) && all(is.finite(actual)) &&
        max(abs(actual - reference)) < 1e-7
    }, logical(1)))
  }, error = function(e) FALSE)
  if (!compatible) {
    stop("Installed neurotransform fails the independent H5 convention probe. ",
         "Install the revision pinned in neuroatlas's Remotes field.")
  }
  invisible(TRUE)
}

#' Apply a Template Transform on an Explicit Grid
#'
#' Composes the complete route in physical coordinates and samples each input
#' channel once. Labels use nearest neighbour; scalar and probability values
#' use linear interpolation. Out-of-field samples are zero. Probability channels
#' are interpolated independently, without clipping or renormalisation.
#'
#' @param x A volumetric atlas, `NeuroVol`, or `NeuroVec` (channels in dimension 4).
#' @param transform A verified [get_template_transform()] result.
#' @param target Target atlas, `NeuroVol`, or explicit `NeuroSpace`. A bare grid
#'   is an assertion by the caller that it is in the transform's target space.
#'   Attached source/target metadata must agree with the route.
#' @param data_type `"auto"` uses declared metadata, or label semantics for atlas
#'   objects. Unannotated volumes require an explicit type; integer-valued
#'   samples alone do not imply labels.
#' @param interpolation `NULL` selects the type-specific method. Only `"nearest"`
#'   for labels and `"linear"` for continuous/probability data are supported.
#' @return The transformed atlas or volume. The `neuroatlas_transform` attribute
#'   records the route, artifact hashes, interpolation, grids and lost labels.
#'   Atlas objects retain semantic IDs, labels and source provenance, including
#'   regions that disappear on the target grid.
#' @export
apply_template_transform <- function(x, transform, target,
                                     data_type = c("auto", "continuous", "label", "probability"),
                                     interpolation = NULL) {
  data_type <- match.arg(data_type)
  .require_neurotransform()
  if (!inherits(transform, "template_transform") ||
      !identical(transform$plan$status, "available") ||
      !identical(transform$from_space, transform$plan$from_space) ||
      !identical(transform$to_space, transform$plan$to_space)) {
    stop("'transform' must be a verified available template_transform.")
  }
  if (inherits(x, "surfatlas") || inherits(target, "surfatlas")) {
    stop("Template transforms currently support volumetric data only.")
  }
  is_atlas <- inherits(x, "atlas")
  source_meta <- if (is_atlas) atlas_metadata(x) else {
    attr(x, "neuroatlas_metadata", exact = TRUE)
  }
  previous <- attr(x, "neuroatlas_transform", exact = TRUE)
  if (!is.null(previous) &&
      !identical(previous$to_space, transform$from_space)) {
    stop("Recorded source template does not match the requested transform.")
  }
  target_meta <- if (inherits(target, "atlas")) atlas_metadata(target) else {
    attr(target, "neuroatlas_metadata", exact = TRUE)
  }
  .check_transform_space(source_meta, transform$from_space, "source")
  .check_transform_space(target_meta, transform$to_space, "target")
  declared_type <- if (is_atlas) "labels" else source_meta$content$value_type
  inferred <- c(labels = "label", mask = "label", intensity = "continuous",
                probability = "probability")[declared_type]
  if (data_type == "auto") {
    if (length(inferred) != 1L || is.na(inferred)) {
      stop("Unannotated data require an explicit 'data_type'.")
    }
    data_type <- unname(inferred)
  } else if (length(inferred) == 1L && !is.na(inferred) &&
             data_type != unname(inferred)) {
    stop("'data_type' conflicts with declared source metadata.")
  }
  method <- if (data_type == "label") "nearest" else "linear"
  if (!is.null(interpolation) && !identical(interpolation, method)) {
    stop("Interpolation for ", data_type, " data must be '", method, "'.")
  }
  moving <- if (is_atlas) .get_atlas_volume(x) else x
  if (!methods::is(moving, "NeuroVol") &&
      !methods::is(moving, "NeuroVec") &&
      !methods::is(moving, "ClusteredNeuroVol")) {
    stop("'x' must contain a NeuroVol or NeuroVec.")
  }
  if (inherits(target, "atlas")) target <- .get_atlas_volume(target)
  target_space <- if (methods::is(target, "NeuroSpace")) target else {
    if (!methods::is(target, "NeuroVol")) {
      stop("'target' must be a NeuroSpace, NeuroVol, or volumetric atlas.")
    }
    neuroim2::space(target)
  }
  if (length(dim(target_space)) != 3L) stop("Target grid must be three-dimensional.")
  values <- if (methods::is(moving, "ClusteredNeuroVol")) {
    methods::as(moving, "array")
  } else as.array(moving)
  if ((!is.numeric(values) && !is.logical(values)) ||
      !length(dim(values)) %in% c(3L, 4L) ||
      any(!is.finite(values))) {
    stop("Input must have finite numeric values on a 3D or 4D grid.")
  }
  if (data_type == "label" && any(values != round(values))) {
    stop("Label data must contain exact integers.")
  }
  if (data_type == "probability" && any(values < 0 | values > 1)) {
    stop("Probability values must lie in [0, 1].")
  }
  storage.mode(values) <- "double"
  if (is.null(source_meta)) {
    source_meta <- .transform_input_metadata(moving, transform$from_space, data_type)
  }
  source_grid <- neurotransform::grid_spec(dim(values)[1:3],
                                          neuroim2::trans(moving))
  target_grid <- neurotransform::grid_spec(dim(target_space),
                                          neuroim2::trans(target_space))
  build_plan <- function() {
    # Hold cache exclusion until all lazy H5 reads have become resident sampling
    # weights. Applying this compiled plan no longer needs the cache files.
    for (i in which(!is.na(transform$files))) {
      path <- .fetch_transform_artifact_unlocked(
        transform$plan$steps[i, , drop = FALSE], transform$cache_dir,
        download = FALSE, offline = TRUE, verify = TRUE)
      if (!identical(path, transform$files[[i]])) stop("Transform cache path changed.")
    }
    neurotransform::make_resampling_plan(
      transform$morphism, source_grid, target_grid, interpolation = method,
      reuse_count = 2L, cache = FALSE)
  }
  sampling <- if (any(!is.na(transform$files))) {
    .with_transform_cache_lock(
      file.path(transform$cache_dir, ".neuroatlas-cache.lock"), NULL, NULL,
      build_plan)
  } else build_plan()
  result_values <- neurotransform::apply_resampling_plan(
    sampling, values, outside = 0, modulate = "none"
  )
  if (any(!is.finite(result_values))) stop("Transform produced non-finite values.")
  lost <- numeric()
  if (data_type == "label") {
    source_ids <- unique(as.numeric(values))
    result_ids <- unique(as.numeric(result_values))
    if (any(result_values != round(result_values)) ||
        !all(result_ids %in% c(0, source_ids))) {
      stop("Transform violated label integrity.")
    }
    lost <- setdiff(source_ids[source_ids != 0], result_ids)
  }
  if (data_type == "probability" &&
      any(result_values < -1e-6 | result_values > 1 + 1e-6)) {
    stop("Transform violated probability bounds.")
  }
  result <- if (length(dim(result_values)) == 4L) {
    sp <- neuroim2::NeuroSpace(dim(result_values),
                              trans = neuroim2::trans(target_space))
    neuroim2::DenseNeuroVec(result_values, sp)
  } else neuroim2::DenseNeuroVol(result_values, target_space)
  receipt <- list(
    from_space = transform$from_space, to_space = transform$to_space,
    artifacts = transform$plan$steps, files = transform$files,
    source_grid = list(dim = dim(values)[1:3], affine = neuroim2::trans(moving)),
    target_grid = list(dim = dim(target_space), affine = neuroim2::trans(target_space)),
    data_type = data_type, interpolation = method, outside = 0,
    renormalized = FALSE, lost_label_ids = lost,
    neurotransform_version = transform$engine_version,
    neurotransform_source_sha = .neurotransform_source_sha(),
    engine_compatibility = "simpleitk-h5-conventions-v1",
    neuroatlas_version = as.character(utils::packageVersion("neuroatlas"))
  )
  attr(result, "neuroatlas_transform") <- receipt
  if (!is.null(source_meta)) {
    meta <- .transformed_resource_metadata(source_meta, result, transform, receipt)
    attr(result, "neuroatlas_metadata") <- meta
  }
  if (is_atlas) {
    x$atlas <- result
    if (!is.null(x$data)) x$data <- result
    x <- .store_atlas_metadata(x, meta)
    attr(x, "neuroatlas_transform") <- receipt
    validate_atlas(x)
    return(x)
  }
  result
}

.check_transform_space <- function(metadata, expected, role) {
  if (is.null(metadata)) return(invisible(NULL))
  validate_resource_metadata(metadata)
  declared <- metadata$spatial$template_space
  if (is.na(declared) || !nzchar(declared) ||
      !identical(.normalize_space_id(declared), expected)) {
    stop("Declared ", role, " template does not match transform: expected ", expected,
         ", found ", declared, ".")
  }
}

.transform_input_metadata <- function(x, from, data_type) {
  geometry <- .resource_geometry(x, "volume")
  geometry$dimensions <- as.integer(dim(x))
  geometry$voxel_size <- as.numeric(neuroim2::spacing(x))[1:3]
  geometry$affine <- neuroim2::trans(x)
  geometry$units <- "mm"
  .new_resource_metadata(
    kind = "template",
    identity = list(id = from, name = "User-supplied volume", family = "template",
      model = from, version = NA_character_, description = NA_character_,
      species = NA_character_, coverage = NA_character_),
    content = list(representation = "volume", value_type = switch(data_type,
      continuous = "intensity", label = "labels", probability = "probability"),
      parameters = list(), regions = NA_integer_, derived = FALSE),
    spatial = c(list(template_space = from,
      coord_space = suppressWarnings(template_to_coord_space(from)),
      resolution = paste0(paste(geometry$voxel_size, collapse = "x"), "mm"),
      density = NA_character_, basis = "user_supplied"), geometry),
    provenance = list(source = "user_supplied", url = NA_character_,
      lineage = "Input template identity asserted by the transform caller.",
      confidence = "uncertain", notes = NA_character_, issues = character()),
    citations = .empty_resource_citations(), artifacts = .empty_atlas_artifacts(),
    history = .empty_atlas_history()
  )
}

.transformed_resource_metadata <- function(meta, result, transform, receipt) {
  parent <- meta
  geometry <- .resource_geometry(result, "volume")
  if (methods::is(result, "NeuroVec")) {
    geometry$dimensions <- as.integer(dim(result))
    geometry$voxel_size <- as.numeric(neuroim2::spacing(result))[1:3]
    geometry$affine <- neuroim2::trans(result)
    geometry$units <- "mm"
  }
  for (nm in names(geometry)) meta$spatial[[nm]] <- geometry[[nm]]
  meta$spatial$template_space <- transform$to_space
  meta$spatial$coord_space <- suppressWarnings(template_to_coord_space(transform$to_space))
  meta$spatial$resolution <- paste0(paste(geometry$voxel_size, collapse = "x"), "mm")
  meta$spatial$basis <- parent$spatial$basis
  meta$spatial$sampling_reference <- transform$to_space
  meta$content$derived <- TRUE
  meta$parents <- list(source = parent)
  meta$history <- dplyr::bind_rows(meta$history, .new_atlas_history(
    "template_transform", "volume", from_template_space = transform$from_space,
    to_template_space = transform$to_space,
    from_coord_space = parent$spatial$coord_space,
    to_coord_space = meta$spatial$coord_space,
    confidence = transform$plan$confidence,
    details = "Applied a verified template route with one sampling operation.",
    parameters = receipt
  ))
  meta$history$step <- seq_len(nrow(meta$history))
  steps <- transform$plan$steps
  for (i in which(!is.na(transform$files))) {
    step <- steps[i, , drop = FALSE]
    artifact <- .new_atlas_artifact(
      role = "template_transform", family = "template", model = step$artifact_id,
      source_name = step$provider, source_url = step$url,
      source_ref = step$artifact_id, source_version = step$artifact_version,
      license = step$license, file_name = basename(transform$files[[i]]),
      local_path = transform$files[[i]], sha256 = step$sha256,
      template_space = step$to_space, confidence = step$confidence
    )
    artifact$checksum <- step$sha256
    artifact$checksum_algorithm <- "sha256"
    artifact$checksum_basis <- "verified_artifact"
    meta$artifacts <- dplyr::bind_rows(meta$artifacts, artifact)
  }
  validate_resource_metadata(meta)
  meta
}

#' Transform a Volumetric Atlas to Another Template
#'
#' Uses atlas metadata to resolve the source space, then applies a verified
#' template transform on the requested target grid. Region identities and source
#' receipts are retained. This changes spatial coordinates, not the atlas's
#' parcellation scheme; use [atlas_overlap()] after alignment to compare parcels.
#'
#' @param x A volumetric atlas with a declared template identity.
#' @param to_space Exact target template identifier.
#' @param target Explicit target atlas, volume or grid. If `NULL`, load the
#'   requested template through [get_template()].
#' @param resolution Template resolution, required when `target` is `NULL`.
#' @param provider Artifact provider.
#' @param ... Download/cache options passed to [get_template_transform()].
#' @return A transformed atlas with updated spatial metadata and history.
#' @export
transform_atlas <- function(x, to_space, target = NULL, resolution = NULL,
                            provider = "auto", ...) {
  if (!inherits(x, "atlas") || inherits(x, "surfatlas")) {
    stop("'x' must be a volumetric atlas.")
  }
  from <- atlas_metadata(x)$spatial$template_space
  transform <- get_template_transform(from, to_space, provider = provider, ...)
  if (is.null(target)) {
    if (is.null(resolution)) stop("Supply 'target' or an explicit 'resolution'.")
    target <- get_template(to_space, resolution = resolution)
  }
  apply_template_transform(x, transform, target, data_type = "label")
}
