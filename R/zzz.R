#' @import reticulate
#' @importFrom rlang :=
.onLoad <- function(libname, pkgname) {
  # Set the RETICULATE_PYTHON_ENV environment variable to use a persistent environment
  # This prevents reticulate from creating ephemeral environments each session
  # Only set if the environment actually exists
  if (Sys.getenv("RETICULATE_PYTHON_ENV") == "") {
    neuroatlas_env <- file.path(tools::R_user_dir("neuroatlas", "config"), "r-reticulate")
    if (dir.exists(neuroatlas_env)) {
      Sys.setenv(RETICULATE_PYTHON_ENV = neuroatlas_env)
    }
  }
  
  # Check if this is first time using neuroatlas with TemplateFlow
  neuroatlas_env <- file.path(tools::R_user_dir("neuroatlas", "config"), "r-reticulate")
  first_time_file <- file.path(tools::R_user_dir("neuroatlas", "config"), ".first_time_complete")
  
}

.onAttach <- function(libname, pkgname) {
  neuroatlas_env <- file.path(tools::R_user_dir("neuroatlas", "config"), "r-reticulate")
  first_time_file <- file.path(tools::R_user_dir("neuroatlas", "config"), ".first_time_complete")

  if (!file.exists(first_time_file) && !dir.exists(neuroatlas_env)) {
    packageStartupMessage(
      "\nWelcome to neuroatlas!\n",
      "To use TemplateFlow features, you'll need to run:\n",
      "  neuroatlas::install_templateflow()\n",
      "This only needs to be done once and will create a persistent environment.\n",
      "For more info, run: neuroatlas::check_templateflow()\n"
    )
  }
}

#' Install Templateflow Python package
#'
#' Installs the Python packages `scipy` and `templateflow` into a persistent
#' virtual environment. This ensures the packages remain available across R sessions.
#'
#' @param method Installation method. Options are "auto" (default), "virtualenv", or "conda".
#'   Using "virtualenv" is recommended for persistent installations.
#' @param conda Path to a conda executable or \code{"auto"} to let
#'   \code{reticulate} locate one. Only used if method = "conda".
#' @param envname Name of the virtual environment. Defaults to "r-neuroatlas" for persistence.
#' @param force_reinstall Logical. If TRUE, forces reinstallation even if packages exist.
#'
#' @return Invisible \code{NULL} indicating success.
#'
#' @details
#' This function creates a persistent Python environment specifically for neuroatlas,
#' preventing the need to reinstall TemplateFlow in each R session. The environment
#' is stored in a user-specific directory that persists across sessions.
#'
#' @examples
#' \donttest{
#' # Install with default settings (recommended)
#' install_templateflow()
#' 
#' # Force reinstallation
#' install_templateflow(force_reinstall = TRUE)
#' }
#' @export
install_templateflow <- function(method = "auto", conda = "auto", 
                               envname = NULL, force_reinstall = FALSE) {
  
  # Determine environment name and path
  if (is.null(envname)) {
    env_path <- file.path(tools::R_user_dir("neuroatlas", "config"), "r-reticulate")
  } else {
    env_path <- envname
  }
  
  # Force virtualenv method if we're in an ephemeral environment
  current_env <- tryCatch(reticulate::py_config()$virtualenv, error = function(e) NULL)
  if (!is.null(current_env) && grepl("r-reticulate-ephemeral", current_env)) {
    message("Note: Currently using an ephemeral Python environment. ",
            "Installing to persistent environment at: ", env_path)
    if (method == "auto") {
      method <- "virtualenv"
      message("Switching to virtualenv method for persistent installation.")
    }
  }
  
  # Check if packages already installed (unless forcing reinstall)
  if (!force_reinstall && method != "conda") {
    # Try to use the environment and check for packages
    tryCatch({
      reticulate::use_virtualenv(env_path, required = FALSE)
      if (reticulate::py_module_available("templateflow") && 
          reticulate::py_module_available("scipy")) {
        message("TemplateFlow and scipy are already installed in: ", env_path)
        return(invisible(NULL))
      }
    }, error = function(e) {
      # Environment doesn't exist or packages not found, continue with installation
    })
  }
  
  # Create virtual environment if it doesn't exist
  if (method == "virtualenv" || (method == "auto" && is.null(reticulate::conda_binary()))) {
    if (!dir.exists(env_path)) {
      # Ensure parent directory exists
      parent_dir <- dirname(env_path)
      if (!dir.exists(parent_dir)) {
        dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
      }
      message("Creating persistent virtual environment at: ", env_path)
      reticulate::virtualenv_create(envname = env_path)
    }
    
    message("Installing packages to persistent environment: ", env_path)
    reticulate::virtualenv_install(envname = env_path, 
                                 packages = c("scipy", "templateflow"),
                                 ignore_installed = force_reinstall)
  } else {
    # Use standard py_install for conda or when virtualenv not available
    message("Installing packages using method: ", method)
    reticulate::py_install(c("scipy", "templateflow"), 
                         method = method, 
                         conda = conda,
                         pip = TRUE,
                         envname = if (!is.null(envname)) envname else NULL)
  }
  
  message("Installation complete. Packages installed to: ", 
          if (method == "virtualenv" || (method == "auto" && is.null(reticulate::conda_binary()))) 
            env_path else "default Python environment")
  
  # Mark first time setup as complete
  config_dir <- tools::R_user_dir("neuroatlas", "config")
  first_time_file <- file.path(config_dir, ".first_time_complete")
  if (!file.exists(first_time_file)) {
    tryCatch({
      # Ensure directory exists before writing file
      if (!dir.exists(config_dir)) {
        dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
      }
      writeLines("", first_time_file)
    }, error = function(e) {
      # Ignore errors writing marker file
    })
  }
  
  invisible(NULL)
}

#' Check TemplateFlow Installation Status
#'
#' Checks whether TemplateFlow and required dependencies are properly installed
#' and provides information about the Python environment being used.
#'
#' @return Invisible NULL. Prints status information to the console.
#' @export
#' @examples
#' \donttest{
#' check_templateflow()
#' }
check_templateflow <- function() {
  cat("=== TemplateFlow Installation Status ===\n\n")
  
  # Check Python availability
  if (!reticulate::py_available(initialize = FALSE)) {
    cat("Python: NOT AVAILABLE\n")
    cat("  Run: reticulate::install_python()\n\n")
    return(invisible(NULL))
  }
  
  cat("Python: Available\n")
  
  # Check Python configuration
  py_config <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  if (!is.null(py_config)) {
    if (!is.null(py_config$version)) {
      if (is.list(py_config$version)) {
        cat("Python version:", paste(py_config$version, collapse=" "), "\n")
      } else {
        cat("Python version:", py_config$version, "\n")
      }
    }
    cat("Python executable:", py_config$python, "\n")
    
    if (!is.null(py_config$virtualenv)) {
      if (grepl("ephemeral", py_config$virtualenv)) {
        cat("Virtual environment: EPHEMERAL (temporary)\n")
        cat("  WARNING: This is a temporary environment!\n")
        cat("  Packages installed here won't persist between sessions.\n")
      } else {
        cat("Virtual environment:", py_config$virtualenv, "\n")
      }
    }
  }
  
  cat("\n")
  
  # Check persistent environment
  neuroatlas_env <- file.path(tools::R_user_dir("neuroatlas", "config"), "r-reticulate")
  if (dir.exists(neuroatlas_env)) {
    cat("Persistent neuroatlas environment: EXISTS\n")
    cat("  Location:", neuroatlas_env, "\n")
  } else {
    cat("Persistent neuroatlas environment: NOT FOUND\n")
    cat("  Would be created at:", neuroatlas_env, "\n")
  }
  
  cat("\n")
  
  # Check module availability
  cat("Module status:\n")
  if (reticulate::py_module_available("scipy")) {
    cat("  scipy: INSTALLED\n")
  } else {
    cat("  scipy: NOT INSTALLED\n")
  }
  
  if (reticulate::py_module_available("templateflow")) {
    cat("  templateflow: INSTALLED\n")
    
    # Try to get version
    tryCatch({
      tf <- reticulate::import("templateflow")
      if (!is.null(tf$`__version__`)) {
        cat("    Version:", tf$`__version__`, "\n")
      }
    }, error = function(e) {
      # Ignore version errors
    })
  } else {
    cat("  templateflow: NOT INSTALLED\n")
  }
  
  cat("\n")
  
  # Check TEMPLATEFLOW_HOME
  tf_home <- Sys.getenv("TEMPLATEFLOW_HOME")
  if (tf_home != "") {
    cat("TEMPLATEFLOW_HOME:", tf_home, "\n")
    if (dir.exists(tf_home)) {
      cat("  Status: Directory exists\n")
    } else {
      cat("  Status: Directory does not exist\n")
    }
  } else {
    cat("TEMPLATEFLOW_HOME: Not set\n")
  }
  
  cat("\n")
  
  # Recommendations
  if (!reticulate::py_module_available("templateflow") || 
      !reticulate::py_module_available("scipy")) {
    cat("=== Recommendation ===\n")
    cat("Run: neuroatlas::install_templateflow()\n")
    cat("This will create a persistent environment and install required packages.\n")
  } else if (!is.null(py_config$virtualenv) && grepl("ephemeral", py_config$virtualenv)) {
    cat("=== Recommendation ===\n")
    cat("You're using a temporary environment. To make it persistent:\n")
    cat("Run: neuroatlas::install_templateflow()\n")
  } else {
    cat("=== Status: Ready ===\n")
    cat("TemplateFlow is properly installed and ready to use.\n")
  }
  
  invisible(NULL)
}
