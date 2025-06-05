#' Print Methods for neuroatlas Objects
#'
#' @description
#' Print methods for various atlas objects in the neuroatlas package
#'
#' @param x An atlas object (atlas, glasser, schaefer, etc.)
#' @param ... Additional arguments passed to print
#'
#' @return The object is returned invisibly
#' @name print-methods
NULL

#' Plot Methods for neuroatlas Objects  
#'
#' @description
#' Plot methods for various atlas objects in the neuroatlas package
#'
#' @param x An atlas object (atlas, glasser, schaefer, etc.)
#' @param y Ignored (required for S3 consistency)
#' @param ... Additional arguments passed to specific plot implementations
#'
#' @return A plot object (specific type depends on the atlas)
#' @name plot-methods
NULL

#' @rdname print-methods
#' @importFrom cli rule symbol
#' @importFrom crayon bold blue green yellow red white
#' @export
print.schaefer <- function(x, ...) {
  cat(cli::rule(crayon::bold("Schaefer Atlas"), line = 2), "\n")
  cat(crayon::blue("Name:"), x$name, "\n")
  
  if (!is.null(x$atlas)) {
    dims <- dim(x$atlas)
    cat(crayon::blue("Dimensions:"), paste(dims, collapse = " x "), "\n")
  }
  
  n_regions <- length(x$ids)
  cat(crayon::blue("Regions:"), n_regions, "\n")
  
  # Extract network count from name
  if (grepl("\\d+networks", x$name)) {
    networks <- sub(".*-(\\d+)networks.*", "\\1", x$name)
    cat(crayon::blue("Networks:"), networks, "\n")
  }
  
  # Hemisphere distribution
  if (!is.null(x$hemi)) {
    hemi_table <- table(x$hemi)
    hemi_str <- paste(names(hemi_table), hemi_table, sep = ": ", collapse = ", ")
    cat(crayon::blue("Hemispheres:"), hemi_str, "\n")
  }
  
  # Network distribution if available
  if (!is.null(x$network)) {
    unique_networks <- length(unique(x$network))
    cat(crayon::blue("Unique networks:"), unique_networks, "\n")
  }
  
  invisible(x)
}