#' @importFrom rlang :=
.onLoad <- function(libname, pkgname) {
  # No Python environment setup needed - using pure R templateflow package
}

.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("templateflow", quietly = TRUE)) {
    packageStartupMessage(
      "\nWelcome to neuroatlas!\n",
      "To use TemplateFlow features, install the R templateflow package:\n",
      "  remotes::install_github('bbuchsbaum/templateflow')\n"
    )
  }
}

#' Install Templateflow (DEPRECATED)
#'
#' @description
#' **DEPRECATED:** TemplateFlow is now accessed via the pure R `templateflow`
#' package. Python is no longer required.
#'
#' Install the R package instead:
#' \code{remotes::install_github("bbuchsbaum/templateflow")}
#'
#' @param method Ignored.
#' @param conda Ignored.
#' @param envname Ignored.
#' @param force_reinstall Ignored.
#'
#' @return Invisible \code{NULL}.
#'
#' @examples
#' \donttest{
#' # Deprecated. Install the R templateflow package instead:
#' # remotes::install_github("bbuchsbaum/templateflow")
#' }
#' @export
install_templateflow <- function(method = "auto", conda = "auto",
                               envname = NULL, force_reinstall = FALSE) {
  lifecycle::deprecate_warn(
    "0.2.0", "install_templateflow()",
    details = paste(
      "Python is no longer required for TemplateFlow access.",
      "Install the pure R package instead:",
      '  remotes::install_github("bbuchsbaum/templateflow")'
    )
  )

  if (!requireNamespace("templateflow", quietly = TRUE)) {
    message("The 'templateflow' R package is not installed.\n",
            'Install it with: remotes::install_github("bbuchsbaum/templateflow")')
  } else {
    message("The 'templateflow' R package is already installed. No action needed.")
  }

  invisible(NULL)
}

#' Check TemplateFlow Installation Status
#'
#' Checks whether the TemplateFlow R package is installed and provides
#' cache information.
#'
#' @return Invisible NULL. Prints status information to the console.
#' @export
#' @examples
#' \donttest{
#' check_templateflow()
#' }
check_templateflow <- function() {
  cat("=== TemplateFlow Installation Status ===\n\n")

  if (requireNamespace("templateflow", quietly = TRUE)) {
    cat("templateflow R package: INSTALLED\n")

    # Try to get version
    tryCatch({
      pkg_ver <- utils::packageVersion("templateflow")
      cat("  Version:", as.character(pkg_ver), "\n")
    }, error = function(e) {
      # Ignore version errors
    })

    # Check cache
    tryCatch({
      cache_path <- templateflow::tf_home()
      cat("  Cache path:", cache_path, "\n")
      if (dir.exists(cache_path)) {
        stats <- templateflow::tf_cache_stats()
        cat("  Cache size:", stats$cache_size_human, "\n")
        cat("  Cache files:", stats$cache_files, "\n")
      }
    }, error = function(e) {
      cat("  Cache info: unavailable (", e$message, ")\n")
    })

    # Check connectivity
    tryCatch({
      spaces <- templateflow::tf_templates()
      cat("\n  Templates available:", length(spaces), "\n")
    }, error = function(e) {
      cat("\n  Templates: Error listing (", e$message, ")\n")
    })

    cat("\n=== Status: Ready ===\n")
    cat("TemplateFlow is properly installed and ready to use.\n")
  } else {
    cat("templateflow R package: NOT INSTALLED\n\n")
    cat("=== Recommendation ===\n")
    cat('Install with: remotes::install_github("bbuchsbaum/templateflow")\n')
  }

  invisible(NULL)
}
