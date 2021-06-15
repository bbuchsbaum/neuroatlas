tflow <<- NULL

#' @import reticulate
.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  #tflow <<- reticulate::import("scipy", delay_load = TRUE)
  tflow <<- try(reticulate::import("templateflow", delay_load=TRUE))
  
}

#' @export
install_templateflow <- function(method = "auto", conda = "auto") {
  reticulate::py_install("scipy", method = method, conda = conda, pip=TRUE)
  reticulate::py_install("templateflow", method = method, conda = conda, pip=TRUE)
}