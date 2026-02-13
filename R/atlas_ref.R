#' Atlas Reference Metadata
#'
#' Structured provenance and space metadata for atlas objects.
#'
#' @param family Atlas family identifier (e.g., `"schaefer"`, `"glasser"`).
#' @param model Atlas model identifier (e.g., `"Schaefer2018"`).
#' @param representation Atlas representation (`"volume"`, `"surface"`, or
#'   `"derived"`).
#' @param template_space Template/grid identifier (e.g.,
#'   `"MNI152NLin2009cAsym"`, `"fsaverage6"`).
#' @param coord_space Coordinate-space identifier (e.g., `"MNI152"`,
#'   `"MNI305"`).
#' @param resolution Optional resolution descriptor (e.g., `"1mm"`).
#' @param density Optional surface density descriptor (e.g., `"41k"`).
#' @param provenance URL, DOI, or short source identifier.
#' @param source Source key used by the loader.
#' @param lineage Optional derivation/projection note.
#' @param confidence Confidence tier: `"exact"`, `"high"`, `"approximate"`, or
#'   `"uncertain"`.
#' @param notes Optional free-text notes.
#'
#' @return An object of class `"atlas_ref"`.
#' @export
new_atlas_ref <- function(family,
                          model,
                          representation = c("volume", "surface", "derived"),
                          template_space = NA_character_,
                          coord_space = NA_character_,
                          resolution = NA_character_,
                          density = NA_character_,
                          provenance = NA_character_,
                          source = NA_character_,
                          lineage = NA_character_,
                          confidence = c("exact", "high", "approximate", "uncertain"),
                          notes = NA_character_) {
  representation <- match.arg(representation)
  confidence <- match.arg(confidence)

  ref <- list(
    family = as.character(family),
    model = as.character(model),
    representation = representation,
    template_space = as.character(template_space),
    coord_space = as.character(coord_space),
    resolution = as.character(resolution),
    density = as.character(density),
    provenance = as.character(provenance),
    source = as.character(source),
    lineage = as.character(lineage),
    confidence = confidence,
    notes = as.character(notes)
  )

  class(ref) <- c("atlas_ref", "list")
  validate_atlas_ref(ref)
}


#' Validate an Atlas Reference
#'
#' @param x Object to validate.
#'
#' @return Invisibly returns `x` when valid.
#' @export
validate_atlas_ref <- function(x) {
  if (!inherits(x, "atlas_ref")) {
    stop("'x' must inherit from class 'atlas_ref'")
  }

  required <- c(
    "family", "model", "representation", "template_space", "coord_space",
    "resolution", "density", "provenance", "source", "lineage",
    "confidence", "notes"
  )
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop("atlas_ref is missing required fields: ",
         paste(missing, collapse = ", "))
  }

  if (!is.character(x$family) || length(x$family) != 1L || !nzchar(x$family)) {
    stop("'family' must be a non-empty character scalar")
  }
  if (!is.character(x$model) || length(x$model) != 1L || !nzchar(x$model)) {
    stop("'model' must be a non-empty character scalar")
  }
  if (!x$representation %in% c("volume", "surface", "derived")) {
    stop("'representation' must be one of: volume, surface, derived")
  }
  if (!x$confidence %in% c("exact", "high", "approximate", "uncertain")) {
    stop("'confidence' must be one of: exact, high, approximate, uncertain")
  }

  invisible(x)
}


#' Atlas Reference Accessor
#'
#' Returns the canonical atlas reference metadata for an atlas object.
#'
#' @param x An atlas object.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `"atlas_ref"`.
#' @export
atlas_ref <- function(x, ...) {
  UseMethod("atlas_ref")
}

#' @rdname atlas_ref
#' @export
atlas_ref.atlas <- function(x, ...) {
  if (!is.null(x$atlas_ref)) {
    return(validate_atlas_ref(x$atlas_ref))
  }

  # Legacy fallback for older atlas objects.
  new_atlas_ref(
    family = class(x)[1],
    model = if (!is.null(x$name)) x$name else class(x)[1],
    representation = if (inherits(x, "surfatlas")) "surface" else "volume",
    template_space = if (!is.null(x$template_space)) {
      x$template_space
    } else if (!is.null(x$space)) {
      x$space
    } else {
      NA_character_
    },
    coord_space = if (!is.null(x$coord_space)) x$coord_space else NA_character_,
    confidence = "uncertain",
    notes = "Inferred from legacy atlas fields."
  )
}

#' @rdname atlas_ref
#' @export
atlas_ref.default <- function(x, ...) {
  stop("No atlas_ref() method for objects of class: ",
       paste(class(x), collapse = ", "))
}


#' Atlas Family Convenience Accessor
#'
#' @param x An atlas object.
#'
#' @return Character scalar.
#' @export
atlas_family <- function(x) {
  atlas_ref(x)$family
}


#' Atlas Template-Space Convenience Accessor
#'
#' @param x An atlas object.
#'
#' @return Character scalar (or `NA_character_`).
#' @export
atlas_space <- function(x) {
  atlas_ref(x)$template_space
}


#' Atlas Coordinate-Space Convenience Accessor
#'
#' @param x An atlas object.
#'
#' @return Character scalar (or `NA_character_`).
#' @export
atlas_coord_space <- function(x) {
  atlas_ref(x)$coord_space
}


#' Print Method for Atlas References
#'
#' @param x An `atlas_ref` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.atlas_ref <- function(x, ...) {
  validate_atlas_ref(x)
  cat("<atlas_ref>\n")
  cat("  family:", x$family, "\n")
  cat("  model:", x$model, "\n")
  cat("  representation:", x$representation, "\n")
  cat("  template_space:", x$template_space, "\n")
  cat("  coord_space:", x$coord_space, "\n")
  cat("  confidence:", x$confidence, "\n")
  invisible(x)
}


#' Attach Atlas Reference Metadata to an Atlas Object
#'
#' @param x Atlas object list.
#' @param ref An `"atlas_ref"` object.
#'
#' @return Atlas object with attached metadata and compatibility aliases.
#' @keywords internal
#' @noRd
.attach_atlas_ref <- function(x, ref) {
  validate_atlas_ref(ref)
  x$atlas_ref <- ref

  # Compatibility aliases for existing workflows.
  if (is.null(x$space) || is.na(x$space) || !nzchar(x$space)) {
    x$space <- ref$template_space
  }
  x$template_space <- ref$template_space
  x$coord_space <- ref$coord_space
  x$confidence <- ref$confidence

  x
}


#' Resolve Template-Space Hint from `outspace`
#'
#' @param outspace User-supplied outspace argument.
#' @param default_space Space label to return when `outspace` is `NULL`.
#'
#' @return Character scalar template-space hint.
#' @keywords internal
#' @noRd
.template_space_from_outspace <- function(outspace, default_space) {
  if (is.null(outspace)) {
    return(default_space)
  }
  if (is.character(outspace) && length(outspace) == 1L) {
    return(as.character(outspace))
  }
  if (is.list(outspace) && !is.null(outspace$space) &&
      is.character(outspace$space) && length(outspace$space) == 1L) {
    return(as.character(outspace$space))
  }
  "custom"
}
