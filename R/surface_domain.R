#' Identify an Exact Surface Vertex Domain
#'
#' Constructs a descriptor from an ordered registration sphere, its triangles,
#' and its cortical mask. Template names and vertex counts alone cannot establish
#' correspondence. This function records identity; it does not establish that a
#' registration is anatomically valid or qualify a resampling method. Array
#' validation does not establish closed-manifold topology or mesh quality.
#'
#' @param template Exact template family identifier, such as `"fsaverage"` or
#'   `"fsLR"`. Aliases are not inferred.
#' @param hemisphere `"L"` or `"R"`.
#' @param density Density identifier, such as `"164k"` or `"32k"`.
#' @param sphere Numeric matrix of sphere coordinates, one vertex per row and
#'   three columns. Coordinates must be finite, nonzero, and approximately
#'   equidistant from the origin (relative radius tolerance 0.001).
#' @param triangles Integer-valued matrix with three vertex indices per row.
#' @param cortex Logical vector in vertex order; `TRUE` includes cortical
#'   vertices, `FALSE` excludes the medial wall. Missing values are disallowed.
#' @param registration Exact identifier for the sphere correspondence frame.
#'   Matching strings record a declaration, not proof of correspondence.
#' @param revision Immutable upstream sphere revision or content identifier.
#' @param vertex_area Optional positive, finite per-vertex areas in square mm,
#'   in the same vertex order. Absence is recorded explicitly.
#' @param index_base Triangle indexing: `"zero"` (GIFTI default) or `"one"` (R).
#'   It is never inferred from the minimum index.
#'
#' @return A `SurfaceDomain` descriptor containing counts, metadata, SHA-256
#'   fingerprints of ordered coordinates, topology, mask, and optional areas,
#'   and a combined `id`. Arrays are not retained. Numeric arrays are hashed as
#'   row-major little-endian doubles, indices as zero-based 32-bit integers,
#'   and masks as 32-bit integers. Row names and R storage mode do not affect
#'   identity; reordering vertices or triangles does. This conservative identity
#'   deliberately distinguishes equivalent meshes with different encodings.
#' @seealso [atlas_transform_plan()]
#' @export
surface_domain <- function(template, hemisphere, density, sphere, triangles,
                           cortex, registration, revision, vertex_area = NULL,
                           index_base = c("zero", "one")) {
  index_base <- match.arg(index_base)
  for (value in list(template, hemisphere, density, registration, revision)) {
    assertthat::assert_that(is.character(value), length(value) == 1L,
                           !is.na(value), nzchar(trimws(value)))
  }
  assertthat::assert_that(hemisphere %in% c("L", "R"))
  assertthat::assert_that(is.matrix(sphere), is.numeric(sphere),
                         ncol(sphere) == 3L, nrow(sphere) >= 3L,
                         all(is.finite(sphere)))
  radius <- sqrt(rowSums(sphere^2))
  if (any(!is.finite(radius)) || min(radius) <= 0 ||
      max(radius) > min(radius) * 1.001) {
    stop("'sphere' must have nonzero, approximately equal radii.")
  }
  assertthat::assert_that(is.matrix(triangles), is.numeric(triangles),
                         ncol(triangles) == 3L, nrow(triangles) > 0L,
                         all(is.finite(triangles)),
                         all(triangles == trunc(triangles)))
  triangles <- triangles - if (index_base == "one") 1L else 0L
  if (any(triangles < 0 | triangles >= nrow(sphere))) {
    stop("Triangle indices are outside the vertex domain.")
  }
  if (any(triangles[, 1] == triangles[, 2] |
          triangles[, 1] == triangles[, 3] |
          triangles[, 2] == triangles[, 3])) {
    stop("Triangles must have three distinct vertex indices.")
  }
  assertthat::assert_that(is.logical(cortex), is.null(dim(cortex)),
                         length(cortex) == nrow(sphere), !anyNA(cortex))
  if (!is.null(vertex_area)) {
    assertthat::assert_that(is.numeric(vertex_area), is.null(dim(vertex_area)),
                           length(vertex_area) == nrow(sphere),
                           all(is.finite(vertex_area)), all(vertex_area > 0))
  }
  descriptor <- list(
    schema = "neuroatlas.surface-domain.v1", template = template,
    hemisphere = hemisphere, density = density, registration = registration,
    revision = revision, n_vertices = nrow(sphere), n_triangles = nrow(triangles),
    coordinates_sha256 = .surface_array_sha(sphere),
    topology_sha256 = .surface_array_sha(triangles, integer = TRUE),
    cortex_sha256 = .surface_array_sha(cortex, integer = TRUE),
    area_sha256 = if (is.null(vertex_area)) NA_character_ else {
      .surface_array_sha(vertex_area)
    },
    area_units = if (is.null(vertex_area)) NA_character_ else "mm2"
  )
  descriptor$id <- .surface_descriptor_id(descriptor)
  structure(descriptor, class = c("SurfaceDomain", "list"))
}

.surface_array_sha <- function(x, integer = FALSE) {
  values <- if (is.matrix(x)) as.vector(t(x)) else as.vector(x)
  bytes <- writeBin(if (integer) as.integer(values) else as.double(values),
                   raw(), size = if (integer) 4L else 8L, endian = "little")
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

.surface_descriptor_id <- function(x) {
  digest::digest(unclass(x)[setdiff(names(x), "id")], algo = "sha256",
                 serializeVersion = 2L)
}

.validate_surface_domain <- function(x) {
  if (!inherits(x, "SurfaceDomain") ||
      !identical(x$schema, "neuroatlas.surface-domain.v1") ||
      !identical(x$id, .surface_descriptor_id(x))) {
    stop("Invalid or modified SurfaceDomain; reconstruct with surface_domain().")
  }
  invisible(x)
}

.surface_domain_space <- function(x) {
  if (x$template == "fsaverage") {
    spaces <- c(`164k` = "fsaverage", `41k` = "fsaverage6", `10k` = "fsaverage5")
    if (x$density %in% names(spaces)) return(unname(spaces[[x$density]]))
  }
  paste(x$template, x$density, sep = "_")
}
