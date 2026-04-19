#' Atlas Constructor and Validator
#'
#' @description
#' A canonical constructor for `atlas` objects shared across loaders.
#'
#' `new_atlas()` and `new_surfatlas()` assemble the list structure that every
#' volumetric and surface atlas in neuroatlas must conform to:
#' `name`, `atlas` (or `lh_atlas`/`rh_atlas`), `ids`, `labels`, `orig_labels`,
#' `hemi`, `network`, `cmap`, and a pre-built `roi_metadata` tibble. They also
#' attach canonical `atlas_ref` identity and provenance metadata via
#' `.attach_atlas_ref()` / `.attach_atlas_provenance()` so every loader produces
#' consistently shaped output.
#'
#' `validate_atlas()` performs cheap structural checks and raises a typed
#' `neuroatlas_error_invalid_atlas` condition via [cli::cli_abort()] when the
#' object is missing required fields or has inconsistent vector lengths.
#'
#' These helpers are the preferred way for loaders to construct atlas objects;
#' external code should continue to use the documented `get_*_atlas()` entry
#' points and rarely needs to call these directly.
#'
#' @param name Human-readable atlas name (e.g. `"Schaefer-200-7networks"`).
#' @param atlas For volumetric atlases, a `NeuroVol` or `ClusteredNeuroVol`
#'   object.
#' @param ids Integer vector of region IDs.
#' @param labels Character vector of region labels (same length as `ids`).
#' @param orig_labels Optional character vector of original/full labels
#'   (defaults to `labels`).
#' @param hemi Optional character vector of hemisphere designations
#'   (`"left"`, `"right"`, or `NA`).
#' @param network Optional character vector of network assignments (for atlases
#'   that define networks, e.g. Schaefer).
#' @param cmap Optional data frame / matrix with three RGB columns.
#' @param subclass Character vector of subclasses prepended to `"atlas"`
#'   (e.g. `c("schaefer", "volatlas")`).
#' @param extra Named list of extra fields to merge into the returned atlas
#'   object (e.g. `list(space = "MNI152NLin6Asym")`). Reserved names are not
#'   overwritten.
#' @param ref An `atlas_ref` object created with [new_atlas_ref()].
#' @param artifacts Optional artifacts tibble built via `.new_atlas_artifact()`.
#' @param history Optional history tibble built via `.new_atlas_history()`.
#' @param lh_atlas,rh_atlas For surface atlases, the per-hemisphere
#'   `LabeledNeuroSurface` objects.
#' @param surf_type Surface type string (e.g. `"pial"`). Surface atlases only.
#' @param surface_space Surface template space (e.g. `"fsaverage6"`). Surface
#'   atlases only.
#'
#' @return A list with class `c(subclass, "atlas")` (or
#'   `c(subclass, "surfatlas", "atlas")` for surface atlases) containing the
#'   canonical atlas fields plus `roi_metadata` and attached provenance.
#'
#' @seealso [new_atlas_ref()], [atlas_provenance()], [roi_metadata()].
#' @name atlas_constructor
#' @keywords internal
NULL


#' @rdname atlas_constructor
#' @keywords internal
new_atlas <- function(name,
                      atlas,
                      ids,
                      labels,
                      orig_labels = NULL,
                      hemi = NULL,
                      network = NULL,
                      cmap = NULL,
                      subclass = character(),
                      extra = list(),
                      ref,
                      artifacts = NULL,
                      history = NULL) {
  ids <- as.integer(ids)
  n <- length(ids)

  # Coerce labels into canonical form and fill optional fields.
  labels <- as.character(labels)
  if (is.null(orig_labels)) {
    orig_labels <- labels
  } else {
    orig_labels <- as.character(orig_labels)
  }
  if (is.null(hemi)) {
    hemi <- rep(NA_character_, n)
  } else {
    hemi <- as.character(hemi)
  }

  ret <- list(
    name = as.character(name),
    atlas = atlas,
    cmap = .normalize_cmap(cmap),
    ids = ids,
    labels = labels,
    orig_labels = orig_labels,
    hemi = hemi,
    network = network
  )

  # Merge extras without clobbering reserved fields.
  reserved <- c(
    "name", "atlas", "cmap", "ids", "labels", "orig_labels", "hemi",
    "network", "roi_metadata", "atlas_ref", "atlas_artifacts",
    "atlas_history", "space", "template_space", "coord_space",
    "confidence"
  )
  if (length(extra) > 0L) {
    extra_names <- names(extra)
    if (is.null(extra_names) || any(!nzchar(extra_names))) {
      cli::cli_abort(
        "All elements of {.arg extra} must be named.",
        class = c("neuroatlas_error_invalid_atlas", "neuroatlas_error")
      )
    }
    drop <- extra_names %in% reserved
    for (nm in extra_names[!drop]) {
      ret[[nm]] <- extra[[nm]]
    }
  }

  ret$roi_metadata <- .build_roi_metadata(ret)

  class(ret) <- c(as.character(subclass), "atlas")

  validate_atlas(ret)

  ret <- .attach_atlas_ref(ret, ref)
  ret <- .attach_atlas_provenance(ret, artifacts = artifacts, history = history)
  ret
}


#' @rdname atlas_constructor
#' @keywords internal
new_surfatlas <- function(name,
                          lh_atlas,
                          rh_atlas,
                          ids,
                          labels,
                          surf_type,
                          surface_space,
                          orig_labels = NULL,
                          hemi = NULL,
                          network = NULL,
                          cmap = NULL,
                          subclass = character(),
                          extra = list(),
                          ref,
                          artifacts = NULL,
                          history = NULL) {
  ids <- as.integer(ids)
  n <- length(ids)
  labels <- as.character(labels)
  if (is.null(orig_labels)) orig_labels <- labels else orig_labels <- as.character(orig_labels)
  if (is.null(hemi)) {
    hemi <- rep(NA_character_, n)
  } else {
    hemi <- as.character(hemi)
  }

  ret <- list(
    surf_type = as.character(surf_type),
    surface_space = as.character(surface_space),
    lh_atlas = lh_atlas,
    rh_atlas = rh_atlas,
    name = as.character(name),
    cmap = .normalize_cmap(cmap),
    ids = ids,
    labels = labels,
    orig_labels = orig_labels,
    hemi = hemi,
    network = network
  )

  if (length(extra) > 0L) {
    extra_names <- names(extra)
    if (is.null(extra_names) || any(!nzchar(extra_names))) {
      cli::cli_abort(
        "All elements of {.arg extra} must be named.",
        class = c("neuroatlas_error_invalid_atlas", "neuroatlas_error")
      )
    }
    reserved <- c(
      "surf_type", "surface_space", "lh_atlas", "rh_atlas", "name", "cmap",
      "ids", "labels", "orig_labels", "hemi", "network", "roi_metadata",
      "atlas_ref", "atlas_artifacts", "atlas_history", "space",
      "template_space", "coord_space", "confidence"
    )
    drop <- extra_names %in% reserved
    for (nm in extra_names[!drop]) {
      ret[[nm]] <- extra[[nm]]
    }
  }

  ret$roi_metadata <- .build_roi_metadata(ret)

  class(ret) <- c(as.character(subclass), "surfatlas", "atlas")

  validate_atlas(ret)

  ret <- .attach_atlas_ref(ret, ref)
  ret <- .attach_atlas_provenance(ret, artifacts = artifacts, history = history)
  ret
}


#' Validate an Atlas Object
#'
#' Cheap structural validator used by [new_atlas()] and [new_surfatlas()].
#' Raises a classed `neuroatlas_error_invalid_atlas` condition via
#' [cli::cli_abort()] when required fields are missing or have inconsistent
#' lengths.
#'
#' @param x An atlas object.
#'
#' @return Invisibly returns `x` when valid.
#' @keywords internal
validate_atlas <- function(x) {
  if (!inherits(x, "atlas")) {
    cli::cli_abort(
      "{.arg x} must inherit from class {.cls atlas}.",
      class = c("neuroatlas_error_invalid_atlas", "neuroatlas_error")
    )
  }

  required <- c("name", "ids", "labels")
  if (inherits(x, "surfatlas")) {
    required <- c(required, "lh_atlas", "rh_atlas", "surf_type", "surface_space")
  } else {
    required <- c(required, "atlas")
  }

  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields) > 0L) {
    cli::cli_abort(
      c(
        "Atlas object is missing required field{?s}:",
        "*" = "{.field {missing_fields}}"
      ),
      class = c("neuroatlas_error_invalid_atlas", "neuroatlas_error")
    )
  }

  n <- length(x$ids)
  length_checks <- list(
    labels = x$labels,
    orig_labels = x$orig_labels,
    hemi = x$hemi,
    network = x$network
  )
  for (nm in names(length_checks)) {
    val <- length_checks[[nm]]
    if (!is.null(val) && length(val) != n) {
      cli::cli_abort(
        c(
          "Inconsistent field lengths in atlas object.",
          "i" = "{.field ids} has length {n}.",
          "x" = "{.field {nm}} has length {length(val)}."
        ),
        class = c("neuroatlas_error_invalid_atlas", "neuroatlas_error")
      )
    }
  }

  invisible(x)
}


#' Normalise a colour map to a 3-column RGB data frame.
#'
#' Historically loaders stored RGB either as a data frame (Schaefer, Glasser)
#' or as a bare matrix (Olsen MTL / hippocampus). `.build_roi_metadata()`
#' uses `[[1]]` to pull the red channel, which silently picks the wrong value
#' for matrices. Normalising once here lets downstream helpers stay simple.
#'
#' @keywords internal
#' @noRd
.normalize_cmap <- function(cmap) {
  if (is.null(cmap)) return(NULL)
  if (is.data.frame(cmap)) return(cmap)
  if (is.matrix(cmap)) {
    out <- as.data.frame(cmap, stringsAsFactors = FALSE)
    if (ncol(out) >= 3L && is.null(colnames(cmap))) {
      names(out)[1:3] <- c("red", "green", "blue")
    }
    return(out)
  }
  cmap
}
