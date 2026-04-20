#' Require a Suggested Package, or Abort
#'
#' A small, uniform gate used at the entry point of any neuroatlas feature
#' that depends on a package declared in `Suggests` rather than `Imports`.
#' Users who haven't installed the optional dependency get a consistent,
#' typed error pointing them at the right `install.packages()` call
#' instead of a deep, confusing "there is no package called ..." deep in
#' the call stack.
#'
#' Returns invisibly on success; aborts with a
#' `neuroatlas_error_missing_suggest` condition on failure.
#'
#' @param pkg Character scalar naming the package to require.
#' @param feature Optional character scalar naming the feature that depends
#'   on `pkg`; included in the error message for context (e.g. `"brain
#'   surface plotting"`).
#'
#' @return Invisibly `TRUE` when the package is available.
#' @keywords internal
#' @noRd
.require_suggest <- function(pkg, feature = NULL) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(TRUE))
  }

  msg <- if (is.null(feature)) {
    c(
      "Package {.pkg {pkg}} is required but not installed.",
      "i" = "Install it with {.code install.packages(\"{pkg}\")}."
    )
  } else {
    c(
      "Package {.pkg {pkg}} is required for {feature} but not installed.",
      "i" = "Install it with {.code install.packages(\"{pkg}\")}."
    )
  }

  cli::cli_abort(
    msg,
    class = c("neuroatlas_error_missing_suggest", "neuroatlas_error")
  )
}
