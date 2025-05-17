tflow <<- NULL

#' @import reticulate
.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  #tflow <<- reticulate::import("scipy", delay_load = TRUE)
  tflow <<- try(reticulate::import("templateflow", delay_load=TRUE))
  
}

#' Install TemplateFlow Python Dependencies
#'
#' @description
#' Installs the TemplateFlow and scipy Python packages using reticulate.
#'
#' @param method Installation method passed to `reticulate::py_install`.
#' @param conda Conda environment to use for installation.
#'
#' @return Invisibly returns `TRUE` when installation succeeds.
#'
#' @examples
#' \donttest{
#' install_templateflow()
#' }
#' @export
install_templateflow <- function(method = "auto", conda = "auto") {
  reticulate::py_install("scipy", method = method, conda = conda, pip=TRUE)
  reticulate::py_install("templateflow", method = method, conda = conda, pip=TRUE)
  invisible(TRUE)
}