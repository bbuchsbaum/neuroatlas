#' Resolve a Qualified Population Volume-to-Cortex Projection
#'
#' Uses pinned CBIG RF-ANTs population registration-fusion sampling coordinates
#' on the exact fsaverage 164k domain. MNI2009c uses the qualified MNI6-to-2009c
#' image pullback at those coordinates, without an intermediate resampled image.
#' fsLR 32k output additionally uses the admitted native surface operator.
#' Population correspondence does not substitute for subject registration.
#' Numerical qualification covers the locked 1 mm and 2 mm source grids and the
#' four pinned target domains. Other grids in the declared frame can be sampled,
#' but have no retained grid-specific qualification. The original CBIG reference
#' and MNI6 images agree on sampled cortical support, but differ elsewhere.
#'
#' @param from Exact `"MNI152NLin6Asym"` or `"MNI152NLin2009cAsym"` identifier.
#'   Generic `"MNI152"` is insufficient.
#' @param to Verified pinned `SurfaceGeometry` for one hemisphere.
#' @param cache_dir Dedicated transform cache directory.
#' @param download Allow downloads of checksum-locked upstream inputs.
#' @param offline Require verified cached inputs and operators.
#' @return A directed `SurfaceProjection`, with resident sampling coordinates,
#'   exact domains, input hashes, engine identity and composition provenance.
#' @examples
#' \dontrun{
#' target <- get_surface_geometry("fsaverage", "164k", "L")
#' projection <- get_surface_projection("MNI152NLin6Asym", target)
#' volume <- get_template("MNI152NLin6Asym", resolution = 2)
#' cortex <- apply_surface_projection(volume, projection,
#'   source_space = "MNI152NLin6Asym", data_type = "continuous"
#' )
#' }
#' @export
get_surface_projection <- function(
  from,
  to,
  cache_dir = transform_cache_path(),
  download = TRUE,
  offline = FALSE
) {
  to <- .validate_surface_geometry(to)
  plan <- atlas_transform_plan(
    from,
    to$domain,
    data_type = "parcel",
    mode = "strict",
    available_only = TRUE
  )
  if (nrow(plan$steps) != 1L || plan$steps$backend != "cbig_registration_fusion") {
    stop("No qualified population cortical projection for these exact endpoints.")
  }
  .registration_fusion_projection(from, to, cache_dir, download, offline)
}

.registration_fusion_projection <- function(
  from,
  to,
  cache_dir,
  download,
  offline
) {
  for (flag in list(download, offline)) {
    assertthat::assert_that(is.logical(flag), length(flag) == 1L, !is.na(flag))
  }
  if (
    !is.character(from) || length(from) != 1L || is.na(from) ||
      !from %in% c("MNI152NLin6Asym", "MNI152NLin2009cAsym")
  ) {
    stop("Registration fusion requires an exact qualified MNI template identity.")
  }
  to <- .validate_surface_geometry(to)
  engine <- .require_surface_engine()
  revision <- "933edddda462593941e167726e8aaa7168ff103a"
  if (!.surface_engine_revision_verified(revision)) {
    stop("Registration fusion requires the qualified pinned engine build.")
  }
  hemi <- to$domain$hemisphere
  base <- get_surface_geometry(
    "fsaverage",
    "164k",
    hemi,
    cache_dir,
    download,
    offline
  )
  surface_operator <- if (identical(base$domain$id, to$domain$id)) {
    NULL
  } else {
    get_template_transform(
      base,
      to,
      cache_dir = cache_dir,
      download = download,
      offline = offline
    )
  }
  lock <- .surface_input_json("projection-inputs-v1.json")
  asset <- lock$assets[[hemi]]
  artifact <- .locked_surface_artifact(asset, "cbig_mat5", "projection-inputs-v1")
  coords <- .fetch_transform_artifact(
    artifact,
    cache_dir,
    download,
    offline,
    .use = function(path) .read_cbig_ras(path, base$domain$n_vertices)
  )
  volume_route <- NULL
  if (from == "MNI152NLin2009cAsym") {
    # Image movement is 2009c -> MNI6; its pullback maps MNI6 sample points
    # into 2009c. Hold cache exclusion through all lazy transform reads.
    warp <- get_template_transform(
      from,
      "MNI152NLin6Asym",
      cache_dir = cache_dir,
      download = download,
      offline = offline
    )
    coords <- .with_transform_cache_lock(
      file.path(warp$cache_dir, ".neuroatlas-cache.lock"),
      NULL,
      NULL,
      function() {
        for (i in which(!is.na(warp$files))) {
          path <- .fetch_transform_artifact_unlocked(
            warp$plan$steps[i, ],
            warp$cache_dir,
            download = FALSE,
            offline = TRUE,
            verify = TRUE
          )
          if (!identical(path, warp$files[[i]])) stop("Warp cache path changed.")
        }
        as.matrix(neurotransform::transform(warp$morphism, coords))
      }
    )
    volume_route <- warp$plan$steps
  }
  specification <- list(
    schema = "neuroatlas.surface-projection.v1",
    method = "cbig_registration_fusion",
    from_space = from,
    target = to$domain,
    sampling_domain = base$domain,
    source_asset = asset,
    volume_route = volume_route,
    engine = engine,
    qualification = lock$qualification,
    qualification_scope = lock$qualification_scope,
    reversible = FALSE,
    source_frame_scope = "CBIG FSL original and MNI6 agree on sampled cortical support"
  )
  .new_surface_projection(
    specification,
    list(coords),
    base$cortex,
    surface_operator
  )
}

#' Declare Aligned White/Pial Ribbon Sampling
#'
#' Declares ordered white and pial coordinates in the same physical frame as
#' the source volume. Alignment and vertex ordering are assertions by the
#' caller;
#' this function does not fit or verify a registration. Inflated display
#' surfaces
#' must not be supplied. Equal-weight nodes include both endpoints. This method
#' differs from Workbench voxel-intersection ribbon mapping.
#'
#' @param domain Exact surface domain for the ordered anatomical coordinates.
#' @param white,pial Numeric vertex-by-three matrices in RAS millimetres.
#' @param cortex Logical inclusion vector matching the domain's cortical mask.
#' @param frame Exact shared physical-frame identifier.
#' @param n_samples Integer node count, at least two. Nodes are equally spaced
#'   along each white-to-pial segment; categorical nodes vote with smallest-key
#' ties.
#' @return A `SurfaceProjection` with coordinate hashes and caller-declared
#'   alignment provenance. Numerical sampling qualification does not establish
#'   anatomical registration accuracy.
#' @examples
#' \dontrun{
#' projection <- ribbon_projection(domain, white, pial, cortex,
#'   frame = "subject-01-scanner-RAS", n_samples = 5
#' )
#' }
#' @export
ribbon_projection <- function(
  domain,
  white,
  pial,
  cortex,
  frame,
  n_samples = 5L
) {
  .validate_surface_domain(domain)
  assertthat::assert_that(
    is.character(frame),
    length(frame) == 1L,
    !is.na(frame),
    nzchar(frame),
    is.numeric(n_samples),
    length(
      n_samples
    ) == 1L,
    is.finite(n_samples),
    n_samples == trunc(n_samples),
    n_samples >= 2L
  )
  for (coords in list(white, pial)) {
    assertthat::assert_that(
      is.matrix(coords),
      is.numeric(coords),
      identical(dim(coords), c(domain$n_vertices, 3L)),
      all(is.finite(coords))
    )
  }
  assertthat::assert_that(
    is.logical(cortex),
    is.null(dim(cortex)),
    !anyNA(
      cortex
    )
  )
  if (!identical(.surface_array_sha(cortex, TRUE), domain$cortex_sha256)) {
    stop("Ribbon cortical mask does not match its declared domain.")
  }
  engine <- .require_surface_engine()
  points <- lapply(
    seq(0, 1, length.out = n_samples),
    function(t) {
      white * (1 - t) + pial * t
    }
  )
  specification <- list(
    schema = "neuroatlas.surface-projection.v1",
    method = "aligned_ribbon_equal_nodes",
    from_space = frame,
    target = domain,
    sampling_domain = domain,
    white_sha256 = .surface_array_sha(white),
    pial_sha256 = .surface_array_sha(pial),
    n_samples = as.integer(n_samples),
    engine = engine,
    qualification = "alignment_caller_declared",
    reversible = FALSE
  )
  .new_surface_projection(specification, points, cortex, NULL)
}

.new_surface_projection <- function(
  specification,
  points,
  cortex,
  surface_operator
) {
  if (
    any(
      !vapply(
        points,
        function(p) {
          is.matrix(p) && ncol(p) == 3L &&
            nrow(p) == specification$sampling_domain$n_vertices &&
            all(is.finite(p))
        },
        logical(1)
      )
    )
  ) {
    stop("Projection coordinates are invalid or outside transform support.")
  }
  result <- structure(
    list(
      specification = specification,
      points = points,
      cortex = cortex,
      surface_operator = surface_operator
    ),
    class = c("SurfaceProjection", "list")
  )
  result$id <- .surface_projection_id(result)
  result
}

.surface_projection_id <- function(x) {
  .surface_hash(list(x$specification, x$points, x$cortex, x$surface_operator))
}

#' Sample a Volume onto an Explicit Cortical Projection
#'
#' Scalar and probability data use trilinear interpolation; labels use nearest
#' voxel centers with lower-index half-voxel ties. Sampling is restricted to the
#' closed voxel-center box; unsupported values are `NA`, and supported zero is
#' preserved. Probability channels retain partial mass and are not normalized
#' across maps. Missingness considers strictly positive contributors only.
#' fsLR composition resolves sampling missingness on fsaverage first, then
#' resolves missing surface contributors with the native operator. Coverage for
#' each stage is retained separately. Ribbon omission combines voxel and node
#' weights
#' within the sampling stage.
#'
#' @param x A 3D/4D numeric array, `NeuroVol`, `NeuroVec`, or volumetric atlas.
#' @param projection A `SurfaceProjection` from [get_surface_projection()] or
#'   [ribbon_projection()].
#' @param source_space Exact frame identifier. Required for unannotated input;
#'   an explicit value must agree with attached metadata and the projection.
#' @param affine For arrays, a finite nonsingular 4-by-4 matrix mapping
#'   zero-based
#'   voxel indices to RAS millimetres. Neuroimaging objects supply their own
#' affine.
#' @param data_type `"auto"`, `"continuous"`, `"label"`, or `"probability"`.
#'   Unannotated input requires an explicit type.
#' @param na_policy `"propagate"`, `"omit"` with finite-weight normalization,
#'   or `"error"` for a missing positive contributor in included cortical
#' support.
#'   Outside-grid samples remain unsupported under every policy.
#' @param label_table Optional key/name/color table preserved on output.
#' @return `SurfaceData` with values, exact target domain, per-map availability,
#'   finite source-weight mass, geometric sampling coverage, lost labels and
#'   directed projection provenance. Ribbon omission normalizes all finite
#'   voxel/node weights together; categorical nodes vote with smallest-key ties.
#' @examples
#' \dontrun{
#' result <- apply_surface_projection(volume, projection,
#'   source_space = "MNI152NLin6Asym", data_type = "probability"
#' )
#' }
#' @export
apply_surface_projection <- function(
  x,
  projection,
  source_space = NULL,
  affine = NULL,
  data_type = c("auto", "continuous", "label", "probability"),
  na_policy = c("propagate", "omit", "error"),
  label_table = NULL
) {
  data_type <- match.arg(data_type)
  na_policy <- match.arg(na_policy)
  if (
    !inherits(projection, "SurfaceProjection") ||
      !identical(projection$id, .surface_projection_id(projection))
  ) {
    stop("Invalid or modified cortical projection.")
  }
  s <- projection$specification
  .validate_surface_domain(s$target)
  .validate_surface_domain(s$sampling_domain)
  if (!identical(.require_surface_engine(), s$engine)) {
    stop("Projection belongs to another engine; rebuild it.")
  }
  if (!"volume_sampler" %in% getNamespaceExports("neurotransform")) {
    stop("Installed engine lacks the volume sampler.")
  }
  is_atlas <- inherits(x, "atlas")
  source_ref <- if (is_atlas) atlas_ref(x) else NULL
  if (inherits(x, "surfatlas")) stop("Projection input must be volumetric.")
  metadata <- if (is_atlas) {
    atlas_metadata(x)
  } else {
    attr(x, "neuroatlas_metadata", exact = TRUE)
  }
  if (!is.null(metadata)) {
    .check_transform_space(metadata, s$from_space, "source")
    declared <- metadata$spatial$template_space
    if (is.null(source_space)) source_space <- declared
    inferred <- c(
      labels = "label",
      mask = "label",
      intensity = "continuous",
      probability = "probability"
    )[metadata$content$value_type]
    if (length(inferred) == 1L && !is.na(inferred)) {
      if (data_type == "auto") {
        data_type <- unname(inferred)
      } else {
        if (data_type != unname(inferred)) stop("Data type conflicts with source metadata.")
      }
    }
  }
  if (is.null(source_space) || !identical(source_space, s$from_space)) {
    stop("Declare the exact source frame matching the cortical projection.")
  }
  if (data_type == "auto") stop("Unannotated projection input requires a data_type.")
  if (is_atlas) {
    if (is.null(label_table) && data_type == "label") {
      label_table <- data.frame(key = x$ids, name = x$labels)
      if (!0 %in% label_table$key) {
        label_table <- rbind(data.frame(key = 0, name = "background"), label_table)
      }
    }
    x <- .get_atlas_volume(x)
  }
  if (methods::is(x, "NeuroVol") || methods::is(x, "NeuroVec")) {
    if (!is.null(affine)) stop("Neuroimaging inputs supply their own affine.")
    affine <- neuroim2::trans(x)
    x <- if (methods::is(x, "ClusteredNeuroVol")) {
      methods::as(x, "array")
    } else {
      neuroim2::as.array(x)
    }
  }
  assertthat::assert_that(
    is.array(x),
    is.numeric(x),
    length(dim(x)) %in% c(3L, 4L),
    all(dim(x) > 0L),
    !any(is.infinite(x))
  )
  .validate_projection_affine(affine)
  if (data_type == "probability" && any(x < 0 | x > 1, na.rm = TRUE)) {
    stop("Probability inputs must lie in [0,1].")
  }
  keys <- x[is.finite(x)]
  if (
    data_type == "label" &&
      any(keys != trunc(keys) | abs(keys) > .Machine$integer.max)
  ) {
    stop("Categorical projection requires integer label keys.")
  }
  if (
    data_type == "label" && !is.null(label_table) &&
      (
        !is.data.frame(label_table) || !"key" %in% names(label_table) ||
          !is.numeric(label_table$key) || anyNA(label_table$key) ||
          anyDuplicated(label_table$key) || !all(keys %in% label_table$key)
      )
  ) {
    stop("label_table must contain unique keys covering every input label.")
  }
  n <- s$sampling_domain$n_vertices
  maps <- if (length(dim(x)) == 4L) dim(x)[[4L]] else 1L
  interpolation <- if (data_type == "label") "nearest" else "linear"
  samples <- lapply(
    projection$points,
    function(coords) {
      .sample_projection_nodes(
        x,
        affine,
        coords,
        projection$cortex,
        interpolation,
        na_policy
      )
    }
  )
  mass <- Reduce(`+`, lapply(samples, `[[`, "mass")) / length(samples)
  geometry <- Reduce(`+`, lapply(samples, function(z) as.numeric(z$inside))) /
    length(samples)
  available <- if (na_policy == "omit") {
    mass > 0
  } else {
    Reduce(`&`, lapply(samples, `[[`, "available"))
  }
  if (length(samples) == 1L) {
    values <- samples[[1L]]$values
  } else if (data_type == "label") {
    values <- matrix(NA_real_, n, maps)
    for (map in seq_len(maps)) {
      nodes <- do.call(cbind, lapply(samples, function(z) z$values[, map]))
      for (i in which(available[, map])) {
        finite <- nodes[i, is.finite(nodes[i, ])]
        votes <- table(finite)
        values[i, map] <- min(as.numeric(names(votes)[votes == max(votes)]))
      }
    }
  } else {
    numerator <- Reduce(
      `+`,
      lapply(
        samples,
        function(z) {
          values <- z$values
          values[!is.finite(values)] <- 0
          values * z$mass
        }
      )
    ) / length(samples)
    values <- numerator / mass
    values[!available] <- NA_real_
  }
  if (data_type == "probability") {
    if (any(values < -1e-12 | values > 1 + 1e-12, na.rm = TRUE)) {
      stop("Projection violated probability bounds.")
    }
    values[is.finite(values) & values < 0] <- 0
    values[is.finite(values) & values > 1] <- 1
  }
  output <- surface_data(
    if (maps == 1L) as.vector(values) else values,
    s$sampling_domain,
    data_type,
    label_table
  )
  output$coverage <- list(
    available = available,
    source_weight_mass = mass,
    geometric_fraction = geometry,
    target_cortex = projection$cortex
  )
  status <- matrix("available", n, maps)
  status[!available] <- "missing"
  status[geometry < 1 & !available] <- "outside"
  status[!projection$cortex, ] <- "target_masked"
  output$coverage$status <- status
  sampling_coverage <- output$coverage
  if (!is.null(projection$surface_operator)) {
    output <- apply_surface_transform(
      output,
      projection$surface_operator,
      na_policy = if (na_policy == "error") "propagate" else na_policy,
      label_method = "aggregate"
    )
  }
  output$provenance <- list(
    method = s$method,
    projection_id = projection$id,
    projection = s,
    source_values_sha256 = .surface_array_sha(x),
    source_resource_metadata = metadata,
    source_atlas_ref = source_ref,
    source_grid = list(dim = dim(x), affine = affine),
    data_type = data_type,
    interpolation = interpolation,
    nearest_tie = "lower_voxel_index",
    na_policy = na_policy,
    probability_channel_normalization = FALSE,
    sampling_coverage = if (
      !is.null(
        projection$surface_operator
      )
    ) {
      sampling_coverage
    } else {
      NULL
    },
    surface_operator_id = if (!is.null(projection$surface_operator)) {
      projection$surface_operator$key
    } else {
      NULL
    },
    lost_label_keys = if (data_type == "label") {
      setdiff(unique(keys), unique(output$values[is.finite(output$values)]))
    } else {
      NULL
    }
  )
  output
}

.validate_projection_affine <- function(affine) {
  if (
    !is.matrix(affine) || !is.numeric(affine) ||
      !identical(dim(affine), c(4L, 4L)) || any(!is.finite(affine)) ||
      !identical(as.numeric(affine[4, ]), c(0, 0, 0, 1)) ||
      abs(det(affine[1:3, 1:3])) <= .Machine$double.eps
  ) {
    stop("Supply a finite nonsingular zero-based voxel-to-RAS affine.")
  }
}

.sample_projection_nodes <- function(
  x,
  affine,
  coords,
  cortex,
  method,
  na_policy
) {
  voxel <- (cbind(coords, 1) %*% t(solve(affine)))[, 1:3, drop = FALSE]
  dims <- dim(x)[1:3]
  inside <- cortex & apply(
    voxel >= 0 &
      sweep(voxel, 2L, dims - 1L, `<=`),
    1L,
    all
  )
  voxel[!inside, ] <- 0
  if (method == "nearest") voxel <- ceiling(voxel - 0.5)
  maps <- if (length(dim(x)) == 4L) dim(x)[[4L]] else 1L
  finite <- !is.na(x)
  filled <- x
  filled[!finite] <- 0
  # Reuse the pinned interpolation kernel in voxel coordinates. Converting once
  # avoids a second affine inversion changing an exact nearest-neighbor tie.
  evaluate <- function(data) {
    matrix(
      neurotransform::volume_sampler(
        data,
        affine = diag(4),
        method = method,
        outside = NA_real_
      )@evaluate(voxel),
      nrow(coords),
      maps
    )
  }
  values <- evaluate(filled)
  mass <- evaluate(array(as.numeric(finite), dim(x)))
  missing <- matrix(FALSE, nrow(coords), maps)
  offsets <- if (method == "nearest") {
    matrix(0L, 1L, 3L)
  } else {
    as.matrix(expand.grid(0:1, 0:1, 0:1))
  }
  base <- floor(voxel)
  fraction <- voxel - base
  for (j in seq_len(nrow(offsets))) {
    delta <- offsets[j, ]
    positive <- if (method == "nearest") {
      rep(TRUE, nrow(coords))
    } else {
      apply(
        ifelse(
          matrix(rep(delta, each = nrow(coords)), ncol = 3L) == 1,
          fraction > 0,
          fraction < 1
        ),
        1L,
        all
      )
    }
    ijk <- sweep(base, 2L, delta, `+`)
    ijk <- pmin(ijk, matrix(rep(dims - 1L, each = nrow(coords)), ncol = 3L))
    index <- 1L + ijk[, 1] + dims[1] * (ijk[, 2] + dims[2] * ijk[, 3])
    for (map in seq_len(maps)) {
      missing[, map] <- missing[, map] |
        (inside & positive & !finite[index + (map - 1L) * prod(dims)])
    }
  }
  if (na_policy == "error" && any(missing)) {
    stop("Missing positive contributor in included cortical sampling support.")
  }
  mass[!inside, ] <- 0
  values <- values / mass
  available <- mass > 0
  if (na_policy != "omit") available <- available & !missing
  values[!available] <- NA_real_
  list(values = values, mass = mass, available = available, inside = inside)
}

# Read only the locked CBIG MAT-v5 real-double 'ras' matrix. No general MAT
# dependency, arbitrary object deserialization or interpolation code is needed.
.read_cbig_ras <- function(path, n_vertices) {
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  if (length(bytes) < 136L || rawToChar(bytes[127:128]) != "IM") {
    stop("Unsupported CBIG MAT encoding.")
  }
  int <- function(b) {
    readBin(
      b,
      integer(),
      n = length(b) %/% 4L,
      size = 4L,
      endian = "little"
    )
  }
  tag <- int(bytes[129:136])
  if (tag[[1L]] != 15L || tag[[2L]] != length(bytes) - 136L) {
    stop("Expected one compressed CBIG MAT matrix.")
  }
  matrix_bytes <- memDecompress(bytes[137:length(bytes)], "gzip")
  outer <- int(matrix_bytes[1:8])
  if (outer[[1L]] != 14L || outer[[2L]] != length(matrix_bytes) - 8L) {
    stop("Invalid CBIG MAT matrix envelope.")
  }
  offset <- 9L
  element <- function() {
    first <- int(matrix_bytes[offset + 0:3])[[1L]]
    small <- first %/% 65536L
    type <- first %% 65536L
    size <- if (small) small else int(matrix_bytes[offset + 4:7])[[1L]]
    start <- offset + if (small) 4L else 8L
    if (size <= 0L || start + size - 1L > length(matrix_bytes)) {
      stop("Invalid CBIG MAT element size.")
    }
    value <- matrix_bytes[seq.int(start, length.out = size)]
    # Advance the offset held only in this reader's enclosing function.
    offset <<- offset + if (small) { # nolint: assignment_linter.
      8L
    } else {
      8L + ceiling(size / 8) *
        8L
    }
    list(type = type, value = value)
  }
  flags <- element()
  dimensions <- element()
  name <- element()
  values <- element()
  if (
    flags$type != 6L || int(flags$value)[[1L]] != 6L ||
      dimensions$type != 5L ||
      !identical(int(dimensions$value), c(3L, as.integer(n_vertices))) ||
      name$type != 1L || rawToChar(name$value) != "ras" || values$type != 9L ||
      length(values$value) != 3L * n_vertices * 8L
  ) {
    stop("CBIG MAT must contain one real-double 3-by-vertex 'ras' matrix.")
  }
  coords <- t(
    matrix(
      readBin(
        values$value,
        double(),
        n = 3L * n_vertices,
        size = 8L,
        endian = "little"
      ),
      nrow = 3L
    )
  )
  if (any(!is.finite(coords))) stop("Non-finite CBIG sampling coordinates.")
  coords
}
