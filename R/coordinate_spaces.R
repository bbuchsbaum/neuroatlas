#' Coordinate Space Transforms for Neuroimaging Templates
#'
#' @description
#' Functions and constants for transforming coordinates between standard
#' neuroimaging coordinate spaces, particularly MNI305 (fsaverage) and
#' MNI152 (common fMRI template space).
#'
#' @details
#' FreeSurfer's fsaverage surfaces are defined in MNI305 (Talairach-like) space,
#' while most modern fMRI pipelines output data in MNI152 space. The difference
#' is approximately 4mm, which matters for accurate volume-to-surface projections.
#'
#' @section Coordinate Spaces:
#' \describe{
#'   \item{MNI305}{FreeSurfer/Talairach space. Native space for fsaverage,
#'     fsaverage5, and fsaverage6 surfaces. Based on 305 subjects with linear
#'     registration to approximate Talairach space.}
#'   \item{MNI152}{ICBM 2009c space. The de facto standard for volumetric fMRI

#'     analysis. Used by FSL, SPM, fMRIPrep, and TemplateFlow's
#'     MNI152NLin2009cAsym template.}
#' }
#'
#' @references
#' FreeSurfer CoordinateSystems documentation:
#' \url{https://surfer.nmr.mgh.harvard.edu/fswiki/CoordinateSystems}
#'
#' Wu et al. (2018). Accurate nonlinear mapping between MNI volumetric and
#' FreeSurfer surface coordinate systems. Human Brain Mapping, 39(9), 3793-3808.
#' \doi{10.1002/hbm.24213}
#'
#' @name coordinate_spaces
NULL


# Coordinate Space Constants -----------------------------------------------

#' Standard Coordinate Space Identifiers
#'
#' Character constants for standard neuroimaging coordinate spaces.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{MNI305}{FreeSurfer/Talairach space (fsaverage native)}
#'   \item{MNI152}{ICBM 2009c space (common fMRI template)}
#'   \item{SCANNER}{Native scanner space (subject-specific)}
#'   \item{UNKNOWN}{Unknown or unspecified space}
#' }
#'
#' @examples
#' coord_spaces$MNI305
#' coord_spaces$MNI152
#'
#' @export
coord_spaces <- list(

MNI305 = "MNI305",
MNI152 = "MNI152",
SCANNER = "Scanner",
UNKNOWN = "Unknown"
)


# Transform Matrices -------------------------------------------------------

#' MNI305 to MNI152 Affine Transform Matrix
#'
#' The canonical 4x4 affine transformation matrix for converting RAS coordinates
#' from MNI305 (fsaverage) space to MNI152 space.
#'
#' @details
#' This matrix is derived from FreeSurfer's \code{mni152.register.dat} file,
#' located at \code{$FREESURFER_HOME/average/mni152.register.dat}.
#'
#' The transform accounts for the approximately 4mm difference between MNI305
#' and MNI152 coordinate systems. It includes small rotation, scaling, and
#' translation components.
#'
#' To apply: for a point \code{p = c(R, A, S)}, compute
#' \code{MNI305_to_MNI152 \%*\% c(p, 1)} and take the first 3 elements.
#'
#' @format A 4x4 numeric matrix (affine transform in homogeneous coordinates).
#'
#' @source FreeSurfer: \code{$FREESURFER_HOME/average/mni152.register.dat}
#' @seealso
#' \url{https://surfer.nmr.mgh.harvard.edu/fswiki/CoordinateSystems}
#' \code{\link{MNI152_to_MNI305}}, \code{\link{transform_coords}}
#'
#' @examples
#' # Transform a single point from MNI305 to MNI152
#' point_305 <- c(10, -20, 35)
#' point_152 <- (MNI305_to_MNI152 %*% c(point_305, 1))[1:3]
#' print(point_152)  # approximately c(10.695, -18.409, 36.137)
#'
#' @export
MNI305_to_MNI152 <- matrix(c(
   0.9975,  0.0146, -0.0130, 0,
  -0.0073,  1.0009, -0.0093, 0,
   0.0176, -0.0024,  0.9971, 0,
  -0.0429,  1.5496,  1.1840, 1
), nrow = 4, byrow = FALSE)  # R uses column-major order


#' MNI152 to MNI305 Affine Transform Matrix
#'
#' The inverse of \code{\link{MNI305_to_MNI152}}, for converting RAS coordinates
#' from MNI152 space back to MNI305 (fsaverage) space.
#'
#' @inherit MNI305_to_MNI152 details
#' @format A 4x4 numeric matrix (affine transform in homogeneous coordinates).
#'
#' @seealso \code{\link{MNI305_to_MNI152}}, \code{\link{transform_coords}}
#'
#' @examples
#' # Transform a point from MNI152 to MNI305
#' point_152 <- c(10.695, -18.409, 36.137)
#' point_305 <- (MNI152_to_MNI305 %*% c(point_152, 1))[1:3]
#' print(point_305)  # approximately c(10, -20, 35)
#'
#' @export
MNI152_to_MNI305 <- solve(MNI305_to_MNI152)


# Transform Lookup ---------------------------------------------------------

#' Get Transform Matrix Between Coordinate Spaces
#'
#' Retrieve the affine transformation matrix for converting coordinates
#' between two standard neuroimaging coordinate spaces.
#'
#' @param from Character string specifying the source coordinate space.
#'   One of "MNI305", "MNI152".
#' @param to Character string specifying the target coordinate space.
#'   One of "MNI305", "MNI152".
#'
#' @return A 4x4 numeric matrix representing the affine transform.
#'   Returns the identity matrix if \code{from == to}.
#'   Returns \code{NULL} with a warning if the transform is not available.
#'
#' @seealso \code{\link{transform_coords}}, \code{\link{MNI305_to_MNI152}}
#'
#' @examples
#' # Get the MNI305 to MNI152 transform
#' xfm <- get_space_transform("MNI305", "MNI152")
#'
#' # Identity for same-space
#' id <- get_space_transform("MNI152", "MNI152")
#' all.equal(id, diag(4))
#'
#' @export
get_space_transform <- function(from, to) {
  # Validate inputs
  valid_spaces <- c("MNI305", "MNI152")

  if (!is.character(from) || length(from) != 1) {
    stop("'from' must be a single character string")
  }
  if (!is.character(to) || length(to) != 1) {
    stop("'to' must be a single character string")
  }

  # Normalize case for flexibility

from <- toupper(from)
to <- toupper(to)

  if (!from %in% valid_spaces) {
    warning("Unknown source space '", from, "'. ",
            "Known spaces: ", paste(valid_spaces, collapse = ", "))
    return(NULL)
  }
  if (!to %in% valid_spaces) {
    warning("Unknown target space '", to, "'. ",
            "Known spaces: ", paste(valid_spaces, collapse = ", "))
    return(NULL)
  }

  # Identity transform
  if (from == to) {
    return(diag(4))
  }

  # Lookup transform
  if (from == "MNI305" && to == "MNI152") {
    return(MNI305_to_MNI152)
  }
  if (from == "MNI152" && to == "MNI305") {
    return(MNI152_to_MNI305)
  }

  # Should not reach here given validation above
  warning("No transform available from '", from, "' to '", to, "'")
  NULL
}


# Coordinate Transformation ------------------------------------------------

#' Transform Coordinates Between Spaces
#'
#' Apply an affine transformation to convert 3D coordinates from one
#' neuroimaging coordinate space to another.
#'
#' @param coords Numeric matrix of 3D coordinates to transform. Can be:
#'   \itemize{
#'     \item An N x 3 matrix (N points, each row is x/y/z)
#'     \item A 3 x N matrix (if \code{coords_as_cols = TRUE})
#'     \item A length-3 vector (single point)
#'   }
#' @param from Character string specifying the source coordinate space.
#'   One of "MNI305", "MNI152".
#' @param to Character string specifying the target coordinate space.
#'   One of "MNI305", "MNI152".
#' @param transform Optional 4x4 affine matrix. If provided, \code{from} and
#'   \code{to} are ignored and this matrix is applied directly.
#' @param coords_as_cols Logical. If \code{TRUE}, coordinates are in columns
#'   (3 x N matrix). If \code{FALSE} (default), coordinates are in rows
#'   (N x 3 matrix).
#'
#' @return Transformed coordinates in the same format as the input.
#'
#' @details
#' The transformation is applied as: \code{p' = M \%*\% [p; 1]} where M is
#' the 4x4 affine matrix and p is each 3D point in homogeneous coordinates.
#'
#' @seealso \code{\link{get_space_transform}}, \code{\link{MNI305_to_MNI152}}
#'
#' @examples
#' # Transform a single point
#' p305 <- c(10, -20, 35)
#' p152 <- transform_coords(p305, from = "MNI305", to = "MNI152")
#' print(p152)
#'
#' # Transform multiple points (N x 3 matrix)
#' points_305 <- rbind(
#'   c(10, -20, 35),
#'   c(0, 0, 0),
#'   c(-30, 15, 50)
#' )
#' points_152 <- transform_coords(points_305, from = "MNI305", to = "MNI152")
#' print(points_152)
#'
#' # Round-trip: MNI305 -> MNI152 -> MNI305
#' p_roundtrip <- transform_coords(
#'   transform_coords(p305, "MNI305", "MNI152"),
#'   "MNI152", "MNI305"
#' )
#' all.equal(p305, p_roundtrip, tolerance = 1e-10)
#'
#' @export
transform_coords <- function(coords, from = NULL, to = NULL,
                             transform = NULL, coords_as_cols = FALSE) {
  # Get transform matrix
  if (is.null(transform)) {
    if (is.null(from) || is.null(to)) {
      stop("Must provide either 'transform' or both 'from' and 'to'")
    }
    transform <- get_space_transform(from, to)
    if (is.null(transform)) {
      stop("No transform available from '", from, "' to '", to, "'")
    }
  } else {
    if (!is.matrix(transform) || !all(dim(transform) == c(4, 4))) {
      stop("'transform' must be a 4x4 matrix")
    }
  }

  # Handle input formats
  input_is_vector <- is.null(dim(coords))

  if (input_is_vector) {
    if (length(coords) != 3) {
      stop("Vector input must have exactly 3 elements (x, y, z)")
    }
    coords <- matrix(coords, nrow = 1)
    coords_as_cols <- FALSE
  }

  # Ensure coords is a matrix
  coords <- as.matrix(coords)

  # Transpose if coords are in columns
  if (coords_as_cols) {
    if (nrow(coords) != 3) {
      stop("With coords_as_cols=TRUE, coords must have 3 rows")
    }
    coords <- t(coords)
  } else {
    if (ncol(coords) != 3) {
      stop("With coords_as_cols=FALSE, coords must have 3 columns")
    }
  }

  n_points <- nrow(coords)

  # Add homogeneous coordinate
  coords_h <- cbind(coords, 1)

  # Apply transform (coords_h is N x 4, transform is 4 x 4)
  # We want: for each row p, compute transform %*% p
  # This is equivalent to: coords_h %*% t(transform)
  result_h <- coords_h %*% t(transform)

  # Extract 3D coordinates (drop homogeneous)
  result <- result_h[, 1:3, drop = FALSE]

  # Return in original format
  if (coords_as_cols) {
    result <- t(result)
  }

  if (input_is_vector) {
    result <- as.vector(result)
  }

  result
}


# Surface Template Space Lookup --------------------------------------------
#' Get Coordinate Space for a Surface Template
#'
#' Determine which standard coordinate space a surface template's vertices
#' are defined in.
#'
#' @param template_id Character string identifying the surface template.
#'   Examples: "fsaverage", "fsaverage5", "fsaverage6", "fsLR".
#'
#' @return Character string indicating the coordinate space:
#'   \itemize{
#'     \item \code{"MNI305"} for fsaverage variants (fsaverage, fsaverage5, fsaverage6)
#'     \item \code{"MNI152"} for fsLR (HCP template)
#'     \item \code{"Unknown"} for unrecognized templates
#'   }
#'
#' @details
#' FreeSurfer's fsaverage surfaces were created using Talairach registration,
#' which targets MNI305 space. All fsaverage density variants (fsaverage,
#' fsaverage5, fsaverage6) share this coordinate system.
#'
#' The HCP's fsLR template was designed to align with MNI152 space, so no
#' transform is needed when working with MNI152 volumetric data.
#'
#' @seealso \code{\link{transform_coords}}, \code{\link{get_space_transform}}
#'
#' @examples
#' get_surface_coordinate_space("fsaverage")   # "MNI305"
#' get_surface_coordinate_space("fsaverage6")  # "MNI305"
#' get_surface_coordinate_space("fsLR")        # "MNI152"
#'
#' @export
get_surface_coordinate_space <- function(template_id) {
  if (!is.character(template_id) || length(template_id) != 1) {
    stop("'template_id' must be a single character string")
  }

  # Normalize for matching
  template_lower <- tolower(template_id)

  # fsaverage family: all in MNI305
  if (grepl("^fsaverage", template_lower)) {
    return(coord_spaces$MNI305)
  }

  # fsLR (HCP): in MNI152
  if (template_lower == "fslr") {
    return(coord_spaces$MNI152)
  }

  # CIVET surfaces are also in MNI152 (ICBM)
  if (grepl("^civet", template_lower) || grepl("^icbm", template_lower)) {
    return(coord_spaces$MNI152)
  }

  # Unknown template
  warning("Unknown surface template '", template_id,
          "'. Returning 'Unknown' coordinate space.")
  coord_spaces$UNKNOWN
}


#' Check if Transform is Needed Between Spaces
#'
#' Convenience function to check whether a coordinate transform is required
#' when working with a surface template and volumetric data in a given space.
#'
#' @param surface_template Character string identifying the surface template
#'   (e.g., "fsaverage", "fsLR").
#' @param volume_space Character string identifying the volumetric data's
#'   coordinate space (e.g., "MNI152", "MNI305").
#'
#' @return Logical. \code{TRUE} if a transform is needed, \code{FALSE} if
#'   the spaces match.
#'
#' @examples
#' # fsaverage + MNI152 volume: transform needed
#' needs_transform("fsaverage", "MNI152")  # TRUE
#'
#' # fsLR + MNI152 volume: no transform needed
#' needs_transform("fsLR", "MNI152")  # FALSE
#'
#' # fsaverage + MNI305 volume: no transform needed
#' needs_transform("fsaverage", "MNI305")  # FALSE
#'
#' @export
needs_transform <- function(surface_template, volume_space) {
  surf_space <- get_surface_coordinate_space(surface_template)

  if (surf_space == coord_spaces$UNKNOWN) {
    warning("Cannot determine if transform is needed for unknown surface template")
    return(NA)
  }

  # Normalize volume_space
  volume_space <- toupper(volume_space)

  surf_space != volume_space
}


#' Transform Surface Vertices to Volume Space
#'
#' Convenience function to transform surface vertex coordinates to match
#' a target volumetric coordinate space.
#'
#' @param vertices Numeric matrix of vertex coordinates (N x 3).
#' @param surface_template Character string identifying the surface template
#'   the vertices come from (e.g., "fsaverage", "fsLR").
#' @param target_space Character string identifying the target coordinate
#'   space (e.g., "MNI152"). Default is "MNI152".
#'
#' @return Transformed vertex coordinates (N x 3 matrix). Returns the input
#'   unchanged if no transform is needed.
#'
#' @details
#' This is a convenience wrapper around \code{\link{transform_coords}} that
#' automatically determines the source space from the surface template.
#'
#' @seealso \code{\link{transform_coords}}, \code{\link{get_surface_coordinate_space}}
#'
#' @examples
#' \dontrun{
#' # Load fsaverage surface
#' surf <- load_surface_template("fsaverage", "white", hemi = "L")
#' verts <- neurosurf::vertices(surf)
#'
#' # Transform vertices to MNI152 for sampling from fMRI volume
#' verts_mni152 <- transform_vertices_to_volume(verts, "fsaverage", "MNI152")
#' }
#'
#' @export
transform_vertices_to_volume <- function(vertices, surface_template,
                                         target_space = "MNI152") {
  # Get source space from template
  source_space <- get_surface_coordinate_space(surface_template)

  if (source_space == coord_spaces$UNKNOWN) {
    warning("Unknown surface template '", surface_template,
            "'. Returning vertices unchanged.")
    return(vertices)
  }

  # Normalize target space
  target_space <- toupper(target_space)

  # Check if transform is needed
  if (source_space == target_space) {
    return(vertices)
  }

  # Apply transform
  transform_coords(vertices, from = source_space, to = target_space)
}
