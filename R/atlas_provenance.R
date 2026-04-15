#' Atlas Provenance Accessors
#'
#' @description
#' Access structured provenance metadata for atlas objects, including the
#' canonical atlas identity, upstream artifacts, and processing history.
#'
#' @name atlas_provenance
NULL


#' Get Full Atlas Provenance
#'
#' @param x An atlas object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of class `"atlas_provenance"` with fields:
#'   \describe{
#'     \item{ref}{Canonical \code{\link{atlas_ref}()} identity metadata.}
#'     \item{artifacts}{A tibble describing upstream files/resources.}
#'     \item{history}{A tibble describing processing steps applied in
#'       \code{neuroatlas}.}
#'   }
#' @export
atlas_provenance <- function(x, ...) {
  UseMethod("atlas_provenance")
}

#' @rdname atlas_provenance
#' @export
atlas_provenance.atlas <- function(x, ...) {
  structure(
    list(
      ref = atlas_ref(x),
      artifacts = atlas_artifacts(x),
      history = atlas_history(x)
    ),
    class = c("atlas_provenance", "list")
  )
}

#' @rdname atlas_provenance
#' @export
atlas_provenance.default <- function(x, ...) {
  stop("No atlas_provenance() method for objects of class: ",
       paste(class(x), collapse = ", "))
}


#' Get Atlas Artifact Metadata
#'
#' @param x An atlas object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble with one row per upstream artifact used to construct the
#'   atlas.
#' @export
atlas_artifacts <- function(x, ...) {
  UseMethod("atlas_artifacts")
}

#' @rdname atlas_provenance
#' @export
atlas_artifacts.atlas <- function(x, ...) {
  if (is.null(x$atlas_artifacts)) {
    return(.empty_atlas_artifacts())
  }
  .normalize_atlas_artifacts(x$atlas_artifacts)
}

#' @rdname atlas_provenance
#' @export
atlas_artifacts.default <- function(x, ...) {
  stop("No atlas_artifacts() method for objects of class: ",
       paste(class(x), collapse = ", "))
}


#' Get Atlas Processing History
#'
#' @param x An atlas object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble with one row per processing step tracked by
#'   \code{neuroatlas}.
#' @export
atlas_history <- function(x, ...) {
  UseMethod("atlas_history")
}

#' @rdname atlas_provenance
#' @export
atlas_history.atlas <- function(x, ...) {
  if (is.null(x$atlas_history)) {
    return(.empty_atlas_history())
  }
  .normalize_atlas_history(x$atlas_history)
}

#' @rdname atlas_provenance
#' @export
atlas_history.default <- function(x, ...) {
  stop("No atlas_history() method for objects of class: ",
       paste(class(x), collapse = ", "))
}


#' @rdname print-methods
#' @param x An \code{atlas_provenance} object.
#' @param ... Unused.
#' @return Invisibly returns \code{x}.
#' @export
print.atlas_provenance <- function(x, ...) {
  cat("<atlas_provenance>\n")
  cat("  family:", x$ref$family, "\n")
  cat("  model:", x$ref$model, "\n")
  cat("  artifacts:", nrow(x$artifacts), "\n")
  cat("  history steps:", nrow(x$history), "\n")
  invisible(x)
}


#' @keywords internal
#' @noRd
.empty_atlas_artifacts <- function() {
  tibble::tibble(
    role = character(),
    family = character(),
    model = character(),
    variant = character(),
    source_name = character(),
    source_url = character(),
    source_ref = character(),
    citation_doi = character(),
    license = character(),
    file_name = character(),
    sha256 = character(),
    template_space = character(),
    coord_space = character(),
    resolution = character(),
    density = character(),
    parcels = character(),
    networks = character(),
    hemi = character(),
    lineage = character(),
    confidence = character(),
    notes = character()
  )
}


#' @keywords internal
#' @noRd
.empty_atlas_history <- function() {
  tibble::tibble(
    step = integer(),
    action = character(),
    representation = character(),
    from_template_space = character(),
    to_template_space = character(),
    from_coord_space = character(),
    to_coord_space = character(),
    status = character(),
    confidence = character(),
    details = character()
  )
}


#' @keywords internal
#' @noRd
.normalize_atlas_artifacts <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  template <- .empty_atlas_artifacts()

  for (nm in names(template)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- character(nrow(out))
    }
    out[[nm]] <- as.character(out[[nm]])
  }

  tibble::as_tibble(out[, names(template), drop = FALSE])
}


#' @keywords internal
#' @noRd
.normalize_atlas_history <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  template <- .empty_atlas_history()

  for (nm in names(template)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- if (nm == "step") integer(nrow(out)) else character(nrow(out))
    }
  }

  out$step <- as.integer(out$step)
  for (nm in setdiff(names(template), "step")) {
    out[[nm]] <- as.character(out[[nm]])
  }

  tibble::as_tibble(out[, names(template), drop = FALSE])
}


#' @keywords internal
#' @noRd
.new_atlas_artifact <- function(role,
                                family,
                                model,
                                variant = NA_character_,
                                source_name = NA_character_,
                                source_url = NA_character_,
                                source_ref = NA_character_,
                                citation_doi = NA_character_,
                                license = NA_character_,
                                file_name = NA_character_,
                                sha256 = NA_character_,
                                template_space = NA_character_,
                                coord_space = NA_character_,
                                resolution = NA_character_,
                                density = NA_character_,
                                parcels = NA_character_,
                                networks = NA_character_,
                                hemi = NA_character_,
                                lineage = NA_character_,
                                confidence = NA_character_,
                                notes = NA_character_) {
  tibble::tibble(
    role = as.character(role),
    family = as.character(family),
    model = as.character(model),
    variant = as.character(variant),
    source_name = as.character(source_name),
    source_url = as.character(source_url),
    source_ref = as.character(source_ref),
    citation_doi = as.character(citation_doi),
    license = as.character(license),
    file_name = as.character(file_name),
    sha256 = as.character(sha256),
    template_space = as.character(template_space),
    coord_space = as.character(coord_space),
    resolution = as.character(resolution),
    density = as.character(density),
    parcels = as.character(parcels),
    networks = as.character(networks),
    hemi = as.character(hemi),
    lineage = as.character(lineage),
    confidence = as.character(confidence),
    notes = as.character(notes)
  )
}


#' @keywords internal
#' @noRd
.new_atlas_history <- function(action,
                               representation,
                               from_template_space = NA_character_,
                               to_template_space = NA_character_,
                               from_coord_space = NA_character_,
                               to_coord_space = NA_character_,
                               status = "available",
                               confidence = NA_character_,
                               details = NA_character_) {
  tibble::tibble(
    step = NA_integer_,
    action = as.character(action),
    representation = as.character(representation),
    from_template_space = as.character(from_template_space),
    to_template_space = as.character(to_template_space),
    from_coord_space = as.character(from_coord_space),
    to_coord_space = as.character(to_coord_space),
    status = as.character(status),
    confidence = as.character(confidence),
    details = as.character(details)
  )
}


#' @keywords internal
#' @noRd
.attach_atlas_provenance <- function(x, artifacts = NULL, history = NULL) {
  x$atlas_artifacts <- if (is.null(artifacts)) {
    .empty_atlas_artifacts()
  } else {
    .normalize_atlas_artifacts(artifacts)
  }

  x$atlas_history <- if (is.null(history)) {
    .empty_atlas_history()
  } else {
    hist <- .normalize_atlas_history(history)
    hist$step <- seq_len(nrow(hist))
    hist
  }

  x
}


#' @keywords internal
#' @noRd
.append_atlas_history <- function(x,
                                  action,
                                  representation = NULL,
                                  from_template_space = NA_character_,
                                  to_template_space = NA_character_,
                                  from_coord_space = NA_character_,
                                  to_coord_space = NA_character_,
                                  status = "available",
                                  confidence = NA_character_,
                                  details = NA_character_) {
  if (is.null(representation)) {
    representation <- if (inherits(x, "surfatlas")) "surface" else "volume"
  }

  row <- .new_atlas_history(
    action = action,
    representation = representation,
    from_template_space = from_template_space,
    to_template_space = to_template_space,
    from_coord_space = from_coord_space,
    to_coord_space = to_coord_space,
    status = status,
    confidence = confidence,
    details = details
  )

  hist <- if (is.null(x$atlas_history)) {
    .empty_atlas_history()
  } else {
    .normalize_atlas_history(x$atlas_history)
  }

  hist <- dplyr::bind_rows(hist, row)
  hist$step <- seq_len(nrow(hist))
  x$atlas_history <- hist
  x
}
