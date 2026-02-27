#' Atlas Hierarchy
#'
#' Extracts hierarchical level information from an atlas, describing
#' how parcels map to higher-level groupings (networks, hemispheres).
#'
#' @param atlas An atlas object.
#'
#' @return A list of class \code{"atlas_hierarchy"} with components:
#'   \describe{
#'     \item{levels}{Character vector of hierarchy level names, from finest
#'       to coarsest (e.g., \code{c("parcel", "network", "hemisphere")}).}
#'     \item{mappings}{Named list of named character vectors. Each element
#'       maps parcel labels to the corresponding grouping at that level.
#'       Names of the list correspond to \code{levels[-1]} (all levels
#'       above parcel).}
#'   }
#'
#' @examples
#' \dontrun{
#' atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
#' h <- atlas_hierarchy(atlas)
#' h$levels
#' # [1] "parcel" "network" "hemisphere"
#' h$mappings$network[1:5]
#' }
#'
#' @export
atlas_hierarchy <- function(atlas) {
  if (!inherits(atlas, "atlas")) {
    stop("'atlas' must be an atlas object")
  }

  levels <- "parcel"
  mappings <- list()

  # Network level (if available)
  if (!is.null(atlas$network) && length(atlas$network) == length(atlas$ids)) {
    levels <- c(levels, "network")
    mappings$network <- stats::setNames(atlas$network, atlas$labels)
  }

  # Hemisphere level (if available and not all NA)
  if (!is.null(atlas$hemi) && !all(is.na(atlas$hemi))) {
    levels <- c(levels, "hemisphere")
    mappings$hemisphere <- stats::setNames(atlas$hemi, atlas$labels)
  }

  structure(
    list(levels = levels, mappings = mappings),
    class = "atlas_hierarchy"
  )
}

#' @export
print.atlas_hierarchy <- function(x, ...) {
  cat("Atlas Hierarchy\n")
  cat("  Levels:", paste(x$levels, collapse = " > "), "\n")
  for (nm in names(x$mappings)) {
    unique_groups <- unique(x$mappings[[nm]])
    unique_groups <- unique_groups[!is.na(unique_groups)]
    cat("  ", nm, ": ", length(unique_groups), " groups\n", sep = "")
  }
  invisible(x)
}
