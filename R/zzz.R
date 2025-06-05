#' @import reticulate
.onLoad <- function(libname, pkgname) {
  # TemplateFlow initialization is now handled by .init_templateflow_api() in template_flow.R
  # This provides better error handling and environment management
}

#' Install Templateflow Python package
#'
#' Installs the Python packages `scipy` and `templateflow` via
#' \code{reticulate::py_install}. Use this helper if the Templateflow
#' resources are required for fetching templates.
#'
#' @param method Installation method passed to
#'   \code{reticulate::py_install}. Defaults to \code{"auto"}.
#' @param conda Path to a conda executable or \code{"auto"} to let
#'   \code{reticulate} locate one.
#'
#' @return Invisible \code{NULL} indicating success.
#'
#' @examples
#' \donttest{
#' install_templateflow()
#' }
#' @export
install_templateflow <- function(method = "auto", conda = "auto") {
  reticulate::py_install("scipy", method = method, conda = conda, pip = TRUE)
  reticulate::py_install("templateflow", method = method, conda = conda,
                         pip = TRUE)
  invisible(NULL)
}
