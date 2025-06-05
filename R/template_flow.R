# New TemplateFlow Core Interface (WIP - Ticket 1)

#' @importFrom tools R_user_dir
#' @importFrom reticulate import py_available py_module_available py_has_attr py_get_attr py_list_attributes py_to_r is_py_object
#' @importFrom memoise memoise
#' @importFrom lifecycle deprecate_warn
#' @importFrom utils modifyList head askYesNo

# Environment and Cache Setup ----

# TemplateFlow API Handle and Configuration
#
# This object acts as a gateway to the Python TemplateFlow API and manages
# neuroatlas-specific configurations like caching.
.tflow_env <- new.env(parent = emptyenv())

#' Get or Create neuroatlas Cache Directory
#'
#' Returns a path to a neuroatlas-specific cache directory. If the directory
#' (or a specified subdirectory) doesn't exist, it will be created.
#' This function uses `tools::R_user_dir` to ensure a user-specific, 
#' OS-appropriate cache location.
#'
#' @param subdir Optional character string. If provided, a subdirectory named
#'   `subdir` will be created/used within the main neuroatlas cache directory.
#' @return A character string representing the path to the cache directory.
#' @keywords internal
.neuroatlas_cache_dir <- function(subdir = NULL) {
  base_cache_dir <- tools::R_user_dir("neuroatlas", "cache")
  
  cache_path <- base_cache_dir
  if (!is.null(subdir) && nzchar(subdir)) {
    cache_path <- file.path(base_cache_dir, subdir)
  }
  
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  return(cache_path)
}

#' Initialize and Get TemplateFlow API Handle
#'
#' Establishes connection to the Python TemplateFlow API via reticulate.
#' Stores the API handle and cache path in an internal environment.
#' It also sets the TEMPLATEFLOW_HOME environment variable to ensure the 
#' Python library uses the neuroatlas-managed cache directory.
#'
#' @param cache_dir The directory to use for caching TemplateFlow files managed by neuroatlas.
#'                  Defaults to `.neuroatlas_cache_dir("templateflow")`.
#'                  If provided, this path will be used for TEMPLATEFLOW_HOME.
#' @param force_reinit Logical, whether to force re-initialization of the Python API handle.
#' @return Invisibly returns the TemplateFlow S3 object.
#' @keywords internal
.init_templateflow_api <- function(cache_dir = NULL, force_reinit = FALSE) {
  # Determine and set the cache directory first, as it influences TEMPLATEFLOW_HOME
  current_tf_home <- Sys.getenv("TEMPLATEFLOW_HOME")
  target_cache_dir <- ""

  if (!is.null(cache_dir)) {
    # User explicitly provided a cache_dir for this initialization
    target_cache_dir <- cache_dir
    .tflow_env$cache_dir <- target_cache_dir # Store for the R object
    if (!dir.exists(target_cache_dir)) {
      dir.create(target_cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
  } else if (!is.null(.tflow_env$cache_dir) && !force_reinit) {
    # Use previously initialized cache_dir if not forcing reinit and no new one provided
    target_cache_dir <- .tflow_env$cache_dir
  } else {
    # Default: use .neuroatlas_cache_dir("templateflow")
    target_cache_dir <- .neuroatlas_cache_dir("templateflow")
    .tflow_env$cache_dir <- target_cache_dir # Store for the R object
  }
  
  # Set TEMPLATEFLOW_HOME before importing the Python module, 
  # but only if it's not already set or if we are forcing a change.
  # If TEMPLATEFLOW_HOME is already set externally, respect it unless cache_dir is given.
  if (current_tf_home == "" || !is.null(cache_dir) || force_reinit) {
      if (current_tf_home != target_cache_dir) {
          Sys.setenv(TEMPLATEFLOW_HOME = target_cache_dir)
          if (!is.null(.tflow_env$api)) { 
            force_reinit <- TRUE
            message("TEMPLATEFLOW_HOME changed to: ", target_cache_dir, ". Re-initializing Python API.")
          }
      }
  } else if (current_tf_home != "" && current_tf_home != target_cache_dir && is.null(cache_dir)) {
    message("Using externally set TEMPLATEFLOW_HOME: ", current_tf_home)
    .tflow_env$cache_dir <- current_tf_home 
    target_cache_dir <- current_tf_home
  }

  if (is.null(.tflow_env$api) || force_reinit) {
    if (!reticulate::py_available(initialize = TRUE)) {
      stop("Python is not available. The 'reticulate' package needs Python to access TemplateFlow.\n",
           "Please ensure Python is installed on your system.\n",
           "You can check Python availability with: reticulate::py_available()")
    }
    if (!reticulate::py_module_available("templateflow")) {
      stop("Python module 'templateflow' is not installed.\n",
           "To install it, run: neuroatlas::install_templateflow()\n",
           "Or manually: reticulate::py_install('templateflow')")
    }
    tryCatch({
      .tflow_env$py_api <- reticulate::import("templateflow.api", convert = TRUE)
    }, error = function(e) {
      stop("Failed to import templateflow.api: ", e$message, "\n",
           "This may be due to network issues or corrupted installation.\n",
           "Try reinstalling with: neuroatlas::install_templateflow()")
    })
  }
  
  invisible(NULL)
}

#' Check TemplateFlow Connectivity
#'
#' Internal function to test if TemplateFlow API is accessible and functioning.
#' This helps distinguish between network issues and other problems.
#'
#' @return Logical. TRUE if TemplateFlow API is accessible, FALSE otherwise.
#' @keywords internal
.check_templateflow_connectivity <- function() {
  # First check if API is initialized
  if (is.null(.tflow_env$py_api)) {
    tryCatch({
      .init_templateflow_api()
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # If still no API, return FALSE
  if (is.null(.tflow_env$py_api)) {
    return(FALSE)
  }
  
  # Try a simple API call to check connectivity
  tryCatch({
    # Try to get the list of templates - this requires network access
    templates <- .tflow_env$py_api$templates()
    # If we got here, connectivity is good
    return(TRUE)
  }, error = function(e) {
    # Check if it's a network-related error
    if (grepl("(URLError|ConnectionError|timeout|network|internet)", e$message, ignore.case = TRUE)) {
      message("TemplateFlow appears to be unreachable. Check your internet connection.")
    }
    return(FALSE)
  })
}

# Memoised TemplateFlow Path Fetching ----

#' Perform TemplateFlow API Call and Convert to Path
#' 
#' Internal function that calls the Python TemplateFlow API's get() method,
#' handles errors, and converts the result (potentially a list of paths)
#' to a single R file path string. If multiple paths are returned by TemplateFlow,
#' a warning is issued and the first path is used.
#'
#' @param tf_api_obj The Python TemplateFlow API object from reticulate.
#' @param query_params_list A named list of query parameters for `tf_api_obj$get()`.
#' @return A single character string representing the file path.
#' @keywords internal
.perform_tf_get_and_convert_to_path <- function(tf_api_obj, query_params_list) {
  py_result_path_obj <- NULL
  
  # Capture Python errors from the API call
  py_result_path_obj <- tryCatch({
    do.call(tf_api_obj$get, query_params_list)
  }, error = function(e) {
    stop(structure(
      list(message = paste0("TemplateFlow API error: ", conditionMessage(e),
                            "\nQuery args: ", paste(names(query_params_list), query_params_list, sep="=", collapse=", ")),
           call = NULL,
           query_args = query_params_list,
           python_error = e),
      class = c("templateflow_api_error", "error", "condition")
    ))
  })

  if (is.null(py_result_path_obj)) {
    stop(structure(
      list(message = paste0("TemplateFlow found no files for the given query.",
                            "\nQuery args: ", paste(names(query_params_list), query_params_list, sep="=", collapse=", ")),
           call = NULL,
           query_args = query_params_list),
      class = c("templateflow_no_files_error", "error", "condition")
    ))
  }

  final_py_path_obj_to_convert <- NULL
  # Check if the result is a Python list object
  if (reticulate::is_py_object(py_result_path_obj) && inherits(py_result_path_obj, "python.builtin.list")) {
    num_paths <- length(py_result_path_obj)
    if (num_paths > 1) {
      first_path_str <- tryCatch(reticulate::py_to_r(py_result_path_obj[[1]]$as_posix()), error = function(e) "<unavailable>")
      warning("TemplateFlow returned multiple files (", num_paths, ") for the query, using the first one: ", first_path_str, call. = FALSE)
      final_py_path_obj_to_convert <- py_result_path_obj[[1]]
    } else if (num_paths == 1) {
      final_py_path_obj_to_convert <- py_result_path_obj[[1]]
    } else { # num_paths == 0
      # This case should ideally be caught by `is.null(py_result_path_obj)` check earlier
      # if TemplateFlow returns NULL for empty list, or an actual empty list.
      # If it's an empty Python list, py_result_path_obj would not be NULL.      
      stop(structure(
        list(message = paste0("TemplateFlow returned an empty list of files for the query.",
                              "\nQuery args: ", paste(names(query_params_list), query_params_list, sep="=", collapse=", ")),
             call = NULL,
             query_args = query_params_list),
        class = c("templateflow_no_files_error", "error", "condition")
      ))
    }
  } else { # Not a Python list, assume it's a single Python Path object or similar
    final_py_path_obj_to_convert <- py_result_path_obj
  }

  if (is.null(final_py_path_obj_to_convert)) {
     # Should not happen if logic above is correct, but as a safeguard:
     stop(structure(
        list(message = paste0("Failed to determine a single path from TemplateFlow's response.",
                              "\nQuery args: ", paste(names(query_params_list), query_params_list, sep="=", collapse=", ")),
             call = NULL,
             query_args = query_params_list),
        class = c("templateflow_processing_error", "error", "condition")
      ))
  }
  
  # Convert the Python Path object to an R string path
  r_path_string <- tryCatch({
    reticulate::py_to_r(final_py_path_obj_to_convert$as_posix())
  }, error = function(e_conv) {
    stop(structure(
        list(message = paste0("Failed to convert Python path object to R string: ", e_conv$message,
                              "\nQuery args: ", paste(names(query_params_list), query_params_list, sep="=", collapse=", ")),
             call = NULL,
             query_args = query_params_list,
             conversion_error = e_conv),
        class = c("templateflow_conversion_error", "error", "condition")
      ))
  })
  
  return(r_path_string)
}

#' Memoised version of .perform_tf_get_and_convert_to_path
#' @keywords internal
.memoised_fetch_templateflow_path <- memoise::memoise(.perform_tf_get_and_convert_to_path)


# TemplateFlow S3 Object and Methods ----

#' Create a TemplateFlow Interface Object
#'
#' Initializes and returns an S3 object of class \code{templateflow} which acts as
#' a gateway to the TemplateFlow Python API and manages configurations.
#'
#' @param cache_dir Optional. Path to a directory for caching TemplateFlow downloads.
#'                  If NULL (default), uses a neuroatlas-specific cache directory obtained
#'                  via \code{.neuroatlas_cache_dir("templateflow")}. 
#'                  This will also set the \code{TEMPLATEFLOW_HOME} environment variable for the Python session.
#' @param verbosity Optional. An integer for verbosity level (not yet implemented).
#' @param default_template Optional. A string for a default template to use (not yet implemented).
#'
#' @return An S3 object of class \code{templateflow} containing:
#'   \itemize{
#'     \item \code{api}: The raw Python TemplateFlow API handle from \code{reticulate}.
#'     \item \code{cache_path}: The R-side cache path being used.
#'     \item \code{options}: A list of user-provided options.
#'   }
#' @export
#' @examples
#' \dontrun{
#'   # Ensure Python and templateflow module are available
#'   if (reticulate::py_available(initialize = TRUE) && 
#'       reticulate::py_module_available("templateflow")) {
#'     tf <- create_templateflow()
#'     print(tf)
#'   } else {
#'     message("Python or templateflow module not available. Skipping example.")
#'   }
#' }
create_templateflow <- function(cache_dir = NULL, verbosity = 0, default_template = NULL) {
  .init_templateflow_api(cache_dir = cache_dir) # Ensures API handle and cache_dir are in .tflow_env
  
  obj <- list(
    api = .tflow_env$py_api, # Direct access to the Python API object
    cache_path = .tflow_env$cache_dir,
    options = list(
      verbosity = verbosity,
      default_template = default_template
    )
  )
  
  class(obj) <- "templateflow"
  return(obj)
}

#' Print a TemplateFlow Object
#'
#' Provides a brief summary of the TemplateFlow interface object.
#'
#' @param x An object of class \code{templateflow}.
#' @param ... Additional arguments (unused).
#' @export
print.templateflow <- function(x, ...) {
  cat("<neuroatlas TemplateFlow Interface>\n")
  cat("  Cache Path: ", x$cache_path, "\n")
  # Basic check if API object seems valid
  api_status <- tryCatch({
    if (!is.null(x$api) && reticulate::py_has_attr(x$api, "get")) {
      "Connected (Python API handle initialized)"
    } else {
      "Disconnected (Python API handle NOT initialized or invalid)"
    }
  }, error = function(e) "Error checking API status")
  cat("  API Status: ", api_status, "\n")
  
  # Attempt to list some available templates (T5.1.3)
  if (api_status == "Connected (Python API handle initialized)") {
    example_templates <- NULL
    tryCatch({
      # Use the new tflow_spaces function, pass the api_handle
      all_templates <- tflow_spaces(api_handle = x) 
      if (!is.null(all_templates) && length(all_templates) > 0) {
        cat("  Available Templates (Examples): ", paste(utils::head(all_templates, 5), collapse=", "))
        if (length(all_templates) > 5) {
          cat(", ... (Total: ", length(all_templates), ")\n", sep="")
        } else {
          cat(" (Total: ", length(all_templates), ")\n", sep="")
        }
      } else if (!is.null(all_templates)) { # empty list returned
        cat("  Available Templates: None found or list is empty.\n")
      } # If NULL, a warning was already issued by tflow_spaces
    }, error = function(e) {
      cat("  Available Templates: Error retrieving list - ", e$message, "\n")
    })
  } else {
    cat("  Available Templates: Cannot list (API not connected).\n")
  }
  
  invisible(x)
}

#' Access Attributes of the TemplateFlow Object
#'
#' Allows R-native access (via \code{$}) to attributes and methods of the 
#' underlying Python TemplateFlow API object.
#'
#' @param x An object of class \code{templateflow}.
#' @param name The name of the attribute or method to access on the Python object.
#' @return The attribute or method from the Python TemplateFlow API object.
#' @export
#' @method $ templateflow
#' @examples
#' \dontrun{
#'   # Ensure Python and templateflow module are available
#'   if (reticulate::py_available(initialize = TRUE) && 
#'       reticulate::py_module_available("templateflow")) {
#'     tf <- create_templateflow()
#'     # Example: Access the 'get' method (it's a Python function)
#'     # print(tf$get) 
#'     # Example: List available templates (calls tf$api$templates())
#'     # print(tf$templates()) 
#'   } else {
#'     message("Python or templateflow module not available. Skipping example.")
#'   }
#' }
`$.templateflow` <- function(x, name) {
  if (is.null(x$api)) {
    stop("TemplateFlow API handle is not initialized. Call create_templateflow() again.")
  }
  if (!reticulate::py_has_attr(x$api, name)) {
    # Before stopping, check if it's a valid template ID to provide a better error or future enhancement
    # For now, just indicate attribute not found on the API object itself.
    available_attrs <- reticulate::py_list_attributes(x$api)
    stop(paste0("No attribute or method named '", name, "' found on the TemplateFlow API object. ",
                "Available attributes: ", paste(available_attrs, collapse=", "))) 
  }
  reticulate::py_get_attr(x$api, name)
}

#' Access Attributes of the TemplateFlow Object using [[
#'
#' Allows R-native access (via \code{[[}) to attributes and methods of the 
#' underlying Python TemplateFlow API object.
#'
#' @param x An object of class \code{templateflow}.
#' @param name The name of the attribute or method to access on the Python object.
#' @return The attribute or method from the Python TemplateFlow API object.
#' @export
#' @method [[ templateflow
#' @examples
#' \dontrun{
#'   # Ensure Python and templateflow module are available
#'   if (reticulate::py_available(initialize = TRUE) && 
#'       reticulate::py_module_available("templateflow")) {
#'     tf <- create_templateflow()
#'     # Example: Access the 'get' method (it's a Python function)
#'     # print(tf[["get"]]) 
#'   } else {
#'     message("Python or templateflow module not available. Skipping example.")
#'   }
#' }
`[[.templateflow` <- function(x, name) {
  # Forward to the $ method for now, as behavior is identical for this object structure
  # This also means any error handling in $ applies here too.
  x$api[[name]]
}

#' List Attributes of the TemplateFlow API Object
#'
#' Lists the names of attributes and methods available on the underlying
#' Python TemplateFlow API object.
#'
#' @param x An object of class \code{templateflow}.
#' @return A character vector of available attribute and method names.
#' @export
#' @method names templateflow
#' @examples
#' \dontrun{
#'   # Ensure Python and templateflow module are available
#'   if (reticulate::py_available(initialize = TRUE) && 
#'       reticulate::py_module_available("templateflow")) {
#'     tf <- create_templateflow()
#'     # print(names(tf)) 
#'   } else {
#'     message("Python or templateflow module not available. Skipping example.")
#'   }
#' }
names.templateflow <- function(x) {
  if (is.null(x$api)) {
    stop("TemplateFlow API handle is not initialized. Call create_templateflow() again.")
  }
  reticulate::py_list_attributes(x$api)
}

#' Fetch a Template from TemplateFlow
#'
#' Unified function to retrieve neuroimaging templates and related files from
#' the TemplateFlow repository. This function provides a more R-native interface
#' to the underlying Python \code{templateflow.api.get()} method.
#'
#' @param space Character string. The primary TemplateFlow identifier for the template space
#'   (e.g., \code{"MNI152NLin2009cAsym"}). Default: \code{"MNI152NLin2009cAsym"}.
#' @param variant Character string. A high-level descriptor for common template types.
#'   Supported: \code{"brain"} (default), \code{"head"}, \code{"mask"}, \code{"probseg"}, \code{"dseg"}.
#'   This is used to infer \code{desc} and sometimes \code{suffix} if they are not explicitly provided.
#' @param modality Character string. The imaging modality or primary suffix for the template file.
#'   Supported: \code{"T1w"} (default), \code{"T2w"}, \code{"mask"} (often used with \code{variant="mask"}).
#'   This is used to infer \code{suffix} if not explicitly provided.
#' @param resolution Numeric or character. The resolution of the template in mm (e.g., \code{1}, \code{2}, \code{"1"}, \code{"01"}). Default: \code{1}.
#' @param cohort Character string. Optional cohort identifier (e.g., \code{" अधोक्षज"}).
#' @param desc Character string. Specific TemplateFlow \code{desc} field. If provided, this overrides
#'   any \code{desc} inferred from \code{variant}.
#' @param label Character string. Specific TemplateFlow \code{label} field (e.g., \code{"GM"}, \code{"WM"}, \code{"CSF"} for \code{variant="probseg"} or \code{variant="dseg"}).
#' @param atlas Character string. Specific TemplateFlow \code{atlas} field (e.g., \code{"Schaefer2018"}).
#' @param suffix Character string. Specific TemplateFlow \code{suffix} field. If provided, this overrides
#'   any \code{suffix} inferred from \code{modality} or \code{variant}.
#' @param extension Character string. The file extension. Default: \code{".nii.gz"}.
#' @param path_only Logical. If \code{TRUE}, returns the file path to the template as a string
#'   instead of loading it as a \code{NeuroVol} object. Default: \code{FALSE}.
#' @param use_cache Logical. (Currently primarily for future R-level memoisation).
#'   TemplateFlow's Python API has its own caching. Default: `TRUE`.
#'   Actual R-level path memoisation is now active.
#' @param api_handle An optional S3 object of class `templateflow` obtained from
#'   `create_templateflow()`. If `NULL` (default), a default instance is created internally.
#' @param ... Additional arguments passed directly to the Python `templateflow.api.get()` method
#'   (e.g., `raise_on_empty = TRUE`). This allows specifying any valid TemplateFlow query 
#'   entity not explicitly listed as a parameter (e.g., `hemi`, `den`).
#'
#' @details 
#' The function performs several pre-flight checks:
#'   - Validates the existence of the specified `space` using `tf$api$templates()`.
#'   - Validates the specified `resolution` against available resolutions for that `space` using `tf$api$resolutions()`.
#'   - These checks issue warnings and may be skipped if the necessary metadata cannot be retrieved from TemplateFlow.
#' 
#' Caching behavior:
#'   - This function uses `memoise` to cache the resolved file paths from TemplateFlow at the R level for the current session.
#'   - The underlying Python TemplateFlow library also maintains its own disk cache, typically configured via the 
#'     `TEMPLATEFLOW_HOME` environment variable (which this package helps manage).
#'
#' @return If any of `space`, `variant`, `modality`, `resolution`, or `label` are vectors
#'   of length > 1 (and only one of them is vectorized per call), a named list of results is returned.
#'   The names of the list elements correspond to the values of the vectorized parameter.
#'   If all parameters are scalar (or vectors of length 1), a single \code{neuroim2::NeuroVol} object
#'   or a file path string is returned directly (depending on \code{path_only}).
#'
#' @importFrom reticulate py_get_attr
#' @importFrom neuroim2 NeuroVol
#' @export
#' @examples
#' \dontrun{
#'   # Ensure Python and templateflow module are available
#'   if (reticulate::py_available(initialize = TRUE) && 
#'       reticulate::py_module_available("templateflow")) {
#'     
#'     # Get default MNI T1w brain template (scalar call)
#'     mni_brain <- get_template()
#'     print(mni_brain)
#' 
#'     # Vectorized call: Get MNI brain and mask variants
#'     # mni_variants <- get_template(variant = c("brain", "mask"))
#'     # print(names(mni_variants))
#'     # print(mni_variants$brain)
#'     # print(mni_variants$mask)
#'
#'     # Vectorized call: Get MNI T1w at 1mm and 2mm resolutions
#'     # mni_resolutions <- get_template(resolution = c(1, 2))
#'     # print(mni_resolutions$`1`)
#'     # print(mni_resolutions$`2`)
#'
#'     # Vectorized call: Get GM and CSF probseg for MNI
#'     # mni_probsegs <- get_template(variant = "probseg", label = c("GM", "CSF"))
#'     # print(mni_probsegs$GM)
#'
#'     # Path only example with vectorization
#'     # mni_mask_paths <- get_template(space = "MNI152NLin2009cAsym", 
#'     #                              variant = "mask", 
#'     #                              resolution = c(1,2), 
#'     #                              path_only = TRUE)
#'     # print(mni_mask_paths)
#'
#'   } else {
#'     message("Python or templateflow module not available. Skipping example.")
#'   }
#' }
get_template <- function(space = "MNI152NLin2009cAsym", 
                         variant = "brain", 
                         modality = "T1w", 
                         resolution = 1,
                         cohort = NULL, 
                         desc = NULL, 
                         label = NULL, 
                         atlas = NULL, 
                         suffix = NULL, 
                         extension = ".nii.gz",
                         path_only = FALSE, 
                         use_cache = TRUE, 
                         api_handle = NULL, 
                         ...) {

  # --- Vectorized Argument Handling (T2.3.1) --- 
  vectorizable_params <- list(space = space, variant = variant, modality = modality, resolution = resolution, label = label)
  vector_lengths <- sapply(vectorizable_params, length)
  is_actually_vectorized <- vector_lengths > 1
  num_vectorized_args <- sum(is_actually_vectorized)

  if (num_vectorized_args > 1) {
    stop("Vectorization is supported for only one of these parameters at a time: 'space', 'variant', 'modality', 'resolution', 'label'.\n         Please use a loop or an apply-family function (e.g., lapply, purrr::map) for combinatorial retrieval.")
  }

  if (num_vectorized_args == 1) {
    vectorized_param_name <- names(is_actually_vectorized[is_actually_vectorized])
    vectorized_values <- vectorizable_params[[vectorized_param_name]]
    
    # Ensure `...` is captured correctly for recursive calls
    dot_args <- list(...)

    results_list <- lapply(vectorized_values, function(current_iter_value) {
      # Construct the argument list for the recursive call to get_template
      recursive_args <- list(
        cohort = cohort, 
        desc = desc, 
        atlas = atlas, 
        suffix = suffix,
        extension = extension, 
        path_only = path_only, 
        use_cache = use_cache,
        api_handle = api_handle
      )
      
      # Set scalar values for parameters that were originally passed as single-length or NULL
      # and set the current iteration value for the one parameter that is being vectorized.
      recursive_args$space      <- if (vectorized_param_name == "space") current_iter_value else space[1]
      recursive_args$variant    <- if (vectorized_param_name == "variant") current_iter_value else variant[1]
      recursive_args$modality   <- if (vectorized_param_name == "modality") current_iter_value else modality[1]
      recursive_args$resolution <- if (vectorized_param_name == "resolution") current_iter_value else resolution[1]
      recursive_args$label      <- if (vectorized_param_name == "label") current_iter_value else label[1]
      
      # Combine with original ... arguments
      final_recursive_args <- c(recursive_args, dot_args)
      
      # Remove any NULL elements that might have been introduced (e.g. if original label was NULL)
      final_recursive_args <- Filter(Negate(is.null), final_recursive_args)
      
      # Make the recursive call. Using do.call ensures arguments are passed correctly.
      # Need to ensure we are calling the correct get_template (e.g. from this package if developing)
      # If this code is inside the neuroatlas package, neuroatlas::get_template is appropriate.
      # If used interactively for testing, get_template might be sufficient if in global env.
      # For safety in package code: use fully qualified name or ensure correct dispatch.
      do.call(neuroatlas::get_template, final_recursive_args)
    })
    
    names(results_list) <- as.character(vectorized_values)
    return(results_list)
  }
  # --- End Vectorized Argument Handling ---

  # --- Original scalar logic continues below ---
  if (is.null(api_handle)) {
    tf <- create_templateflow() 
  } else {
    if (!inherits(api_handle, "templateflow")) {
      stop("'api_handle' must be an object of class 'templateflow' from create_templateflow().")
    }
    tf <- api_handle
  }

  if (is.null(tf$api)) {
    stop("TemplateFlow API handle is not initialized. Python or 'templateflow' module might be missing.")
  }

  # Pre-flight check: Validate the template space (T4.1.1)
  # Note: tf$api$templates() returns Python list, needs conversion for `is.element`
  available_templates_py <- NULL
  tryCatch({
    available_templates_py <- tf$api$templates()
  }, error = function(e) {
    warning(paste0("Could not retrieve list of available TemplateFlow templates: ", e$message, ". Proceeding without space validation."))
  })
  
  if (!is.null(available_templates_py)){
    available_templates_r <- reticulate::py_to_r(available_templates_py)
    if (!is.element(space, available_templates_r)) {
      stop(paste0("Template space '", space, "' not found in TemplateFlow. \n",
                  "Available top-level template spaces are: ", paste(available_templates_r, collapse=", "), ".\n",
                  "Please check the space identifier or consult https://www.templateflow.org/usage/templateflow-archive/"))
    }
  }

  # Pre-flight check: Validate resolution for the given space (T4.1.2)
  # tf$api$resolutions(space) returns Python list of strings
  if (!is.null(resolution)) { # Only check if resolution is provided
    available_resolutions_py <- NULL
    tryCatch({
      available_resolutions_py <- tf$api$resolutions(space)
    }, error = function(e) {
      warning(paste0("Could not retrieve available resolutions for space '", space, "': ", e$message, ". Proceeding without resolution validation."))
    })

    if (!is.null(available_resolutions_py)) {
      available_resolutions_r <- reticulate::py_to_r(available_resolutions_py)
      # Ensure comparison is between character versions
      requested_res_char <- as.character(resolution)
      
      # TemplateFlow resolutions might be '1', '2' or '01', '02'. 
      # We should be flexible. For now, assume direct string match works.
      # If user provides 1 and TF has "01", this might fail. 
      # The query_args construction already does as.character(resolution), so let's test that.
      # A more robust check might involve converting both to numeric if possible or padding user input.
      # For now, direct check:
      if (!is.element(requested_res_char, available_resolutions_r)) {
        # Attempt to check if integer comparison works (e.g. user says 1, TF has "1")
        # Or if padded version works (user says 1, TF has "01")
        padded_requested_res <- sprintf("%02d", as.integer(requested_res_char))
        if (!(is.element(requested_res_char, available_resolutions_r) || 
             (suppressWarnings(!is.na(as.integer(requested_res_char))) && is.element(padded_requested_res, available_resolutions_r)) ) ) {
          stop(paste0("Resolution '", requested_res_char, "' not found for template space '", space, "'. \n",
                      "Available resolutions are: ", paste(available_resolutions_r, collapse=", "), "."))
        }
      }
    }
  }

  # Construct query parameters for Python API
  query_args <- list()

  # Handle resolution: TemplateFlow expects it as string, potentially with leading zero for <10.
  query_args$resolution <- sprintf("%02d", as.integer(resolution)) 
  # More robust way if TF handles numeric directly or expects string without padding:
  # query_args$resolution <- as.character(resolution) 
  # Check TF documentation for exact format (e.g. '1' vs '01')
  # For now, assuming TemplateFlow Python API handles string or numeric resolution consistently or we pass string.
  # Let's stick to string as per `templateflow.api.get` doc which uses strings for resolution.
  query_args$resolution <- as.character(resolution)

  # --- Parameter Validation (T2.1.3) --- 
  supported_variants <- c("brain", "head", "mask", "probseg", "dseg")
  if (!is.null(variant) && !(variant %in% supported_variants)) {
    warning(paste0("Unsupported 'variant': ", variant, ". It might not be used for inferring 'desc' or 'suffix'. Consider providing 'desc' and 'suffix' explicitly. Supported variants for inference: ", paste(supported_variants, collapse=", "), "."))
  }
  # Add more supported modalities if needed for inference logic
  supported_modalities <- c("T1w", "T2w", "mask") 
  if (!is.null(modality) && !(modality %in% supported_modalities)) {
    warning(paste0("Unsupported 'modality': ", modality, ". It might not be used for inferring 'suffix'. Consider providing 'suffix' explicitly. Supported modalities for inference: ", paste(supported_modalities, collapse=", "), "."))
  }

  # Prioritize explicit desc and suffix
  current_desc <- desc
  current_suffix <- suffix

  # Infer desc from variant if desc is not provided
  if (is.null(current_desc) && !is.null(variant)) {
    current_desc <- switch(variant,
      "brain" = "brain",
      "head" = "head",
      "mask" = "brain", 
      "probseg" = "probseg",
      "dseg" = "dseg",
      NULL 
    )
  }
  
  # Infer suffix from modality/variant if suffix is not provided
  if (is.null(current_suffix) && !is.null(modality)) {
    current_suffix <- switch(modality,
      "T1w" = "T1w",
      "T2w" = "T2w",
      "mask" = "mask", 
      NULL 
    )
  }
  if (is.null(current_suffix) && !is.null(variant)){
      current_suffix <- switch(variant,
          "probseg" = "probseg", # Suffix often same as desc for these
          "dseg" = "dseg",
          "mask" = "mask", # If modality didn't set it as mask
          NULL
      )
  }

  # Validation Check 1: Essential desc and suffix after inference
  final_desc_to_use <- ifelse(!is.null(desc), desc, current_desc)
  final_suffix_to_use <- ifelse(!is.null(suffix), suffix, current_suffix)

  if (is.null(final_desc_to_use)) {
    stop("Could not determine 'desc' parameter. Provide 'desc' explicitly or use a supported 'variant' for inference: ", paste(supported_variants, collapse=", "))
  }
  if (is.null(final_suffix_to_use)) {
    stop("Could not determine 'suffix' parameter. Provide 'suffix' explicitly or use a supported 'variant'/'modality' for inference: ", paste(c(supported_variants, supported_modalities), collapse=", "))
  }

  # Validation Check 2: Label consistency
  if (!is.null(label)) {
    expected_descs_for_label <- c("probseg", "dseg")
    if (!(final_desc_to_use %in% expected_descs_for_label)) {
      warning(paste0("'label' parameter is typically used when 'desc' is one of '", 
                     paste(expected_descs_for_label, collapse="', '"), 
                     "'. Current effective 'desc' is '", final_desc_to_use, "'."))
    }
  }

  # Validation Check 3: variant="mask" consistency
  if (!is.null(variant) && variant == "mask") {
    if (final_suffix_to_use != "mask") {
      warning(paste0("For 'variant = \"mask\"', the typical 'suffix' is \"mask\". ",
                     "Current effective 'suffix' is '", final_suffix_to_use, "'. The result may not be a standard mask file."))
    }
    # Also, for variant="mask", desc is often "brain" or just "mask". 
    # Current inference sets current_desc to "brain" for variant="mask". This is generally fine.
    # If desc was explicitly provided as something else, that takes precedence.
  }

  query_args$desc <- final_desc_to_use
  query_args$suffix <- final_suffix_to_use

  # Add other direct pass-through parameters if not NULL
  if (!is.null(space)) query_args$template <- space # Python API uses 'template' for space ID
  if (!is.null(label)) query_args$label <- label
  if (!is.null(atlas)) query_args$atlas <- atlas
  if (!is.null(cohort)) query_args$cohort <- cohort
  if (!is.null(extension)) query_args$extension <- extension

  # Combine with any additional arguments from ...
  # Ensure ... args don't overwrite explicitly set ones (though tf$api$get might handle this)
  additional_args <- list(...)
  for (arg_name in names(additional_args)) {
    query_args[[arg_name]] <- additional_args[[arg_name]]
  }
  
  # Remove NULLs from query_args before calling, as python function might not like None for all args
  query_args <- Filter(Negate(is.null), query_args)

  # Sort query_args by name to ensure canonical form for memoisation key
  if (length(query_args) > 0) {
    query_args <- query_args[sort(names(query_args))]
  }

  # Call the memoised function to fetch the template path
  file_path_r_string <- tryCatch({
    .memoised_fetch_templateflow_path(tf_api_obj = tf$api, query_params_list = query_args)
  }, templateflow_api_error = function(e) {
    # Stop with the message from the custom error object
    stop(conditionMessage(e))
  }, templateflow_no_files_error = function(e) {
    stop(conditionMessage(e))
  }, templateflow_processing_error = function(e) {
    stop(conditionMessage(e))
  }, templateflow_conversion_error = function(e) {
    stop(conditionMessage(e))
  }, error = function(e) {
    # Catch any other unexpected errors during the call
    stop(paste0("An unexpected error occurred while trying to fetch from TemplateFlow: ", conditionMessage(e)))
  })
  
  # file_path_r_string now holds the path (from cache or fresh query)
  # The original logic for handling py_result_path being NULL or a list is now inside .perform_tf_get_and_convert_to_path

  if (path_only) {
    return(file_path_r_string)
  } else {
    # Convert to NeuroVol using the helper
    return(as_neurovol(file_path_r_string)) # as_neurovol can also take py_result_path directly
  }
}

#' Internal Helper to Convert Python Object/Path to NeuroVol
#'
#' This function takes a file path (string) or a Python object that can be resolved
#' to a NIfTI file path (e.g., a Python Path object from TemplateFlow) and reads it
#' into a \code{neuroim2::NeuroVol} object.
#'
#' @param path_or_py_obj A file path string or a Python object (e.g., \code{pathlib.Path}).
#' @return A \code{neuroim2::NeuroVol} object.
#' @importFrom neuroim2 read_vol
#' @importFrom reticulate py_to_r
#' @keywords internal
.as_neurovol_unmemoised <- function(path_or_py_obj) {
  file_path <- NULL
  if (is.character(path_or_py_obj)) {
    file_path <- path_or_py_obj
  } else if (reticulate::is_py_object(path_or_py_obj) && reticulate::py_has_attr(path_or_py_obj, "as_posix")) {
    # Assuming it's a pathlib.Path-like object from templateflow's get()
    file_path <- reticulate::py_to_r(path_or_py_obj$as_posix())
  } else if (reticulate::is_py_object(path_or_py_obj)){
    # Fallback for other Python objects that might be string paths
    file_path <- tryCatch(reticulate::py_to_r(path_or_py_obj), error = function(e) NULL)
    if (!is.character(file_path)){
        stop("Input 'path_or_py_obj' is a Python object but could not be converted to a file path string.")
    }
  } else {
    stop("Input 'path_or_py_obj' must be a file path string or a suitable Python object.")
  }

  if (!file.exists(file_path)) {
    stop("Resolved file path does not exist: ", file_path)
  }
  
  # Read the NIfTI file using neuroim2::read_vol
  vol <- tryCatch({
    neuroim2::read_vol(file_path)
  }, error = function(e) {
    stop("Failed to read NIfTI file (", file_path, ") into NeuroVol: ", e$message)
  })
  
  return(vol)
}

#' Memoised version of .as_neurovol_unmemoised
#' This is the function that should be called by get_template
#' @keywords internal
as_neurovol <- memoise::memoise(.as_neurovol_unmemoised)

#' Access Templateflow Brain Templates (DEPRECATED - Legacy Signature)
#'
#' @description
#' \\strong{DEPRECATED}: This function signature is deprecated. Please use the new 
#' \code{\link{get_template}} function which offers a more comprehensive 
#' and R-native interface. The new function handles common variants and 
#' modalities more directly.
#'
#' @param name Character string specifying template name. Default: "MNI152NLin2009cAsym"
#' @param desc Character string describing template variant. Default: "brain"
#' @param resolution Numeric resolution in mm. Default: 1
#' @param label Character string specifying tissue label for probability maps
#' @param atlas Character string specifying atlas name
#' @param suffix Character string specifying image type. Default: "T1w"
#' @param extension Character string specifying file extension. Default: ".nii.gz"
#'
#' @return A NeuroVol object containing the requested template
#'
#' @seealso The new \code{\link{get_template}} with updated signature.
#' @md
#' @keywords internal
#' @export
#' @rdname get_template_legacy
get_template_legacy <- function(name="MNI152NLin2009cAsym", desc="brain", resolution=1, 
                               label=NULL, atlas=NULL, suffix="T1w", 
                               extension=".nii.gz") {
  # T8.1.3: Add lifecycle deprecation warning
  lifecycle::deprecate_warn(
    when = "0.10.0", # Replace with actual version when this is released
    what = "get_template(name)",
    with = "get_template(space)",
    details = paste0(
      "The signature get_template(name, desc, resolution, ...) is deprecated.\n",
      "Please use the new signature: get_template(space, variant, modality, resolution, ...), where 'space' replaces 'name'."
    )
  )
  
  # Note: The function signature above is the OLD one.
  # We are calling the NEW get_template() which is defined *earlier* in this file.
  # Need to ensure `neuroatlas::` is not strictly necessary if this file is sourced in order,
  # but explicit call can be safer during refactoring if file is split or order changes.
  # For now, direct call as it *should* resolve to the newer one due to R's environment lookup.
  
  # The new get_template will use explicit desc/suffix if provided, 
  # overriding variant/modality inference from its own defaults.
  neuroatlas::get_template(space = name, 
                           desc = desc, 
                           resolution = resolution, 
                           label = label, 
                           atlas = atlas, 
                           suffix = suffix, 
                           extension = extension,
                           # Pass through other defaults from the new get_template explicitly if needed,
                           # or rely on the new get_template's defaults for variant, modality etc.
                           # when desc/suffix are specific enough.
                           variant = NULL, # Explicitly NULL so new get_template doesn't use its default variant logic if desc is set
                           modality = NULL # Explicitly NULL so new get_template doesn't use its default modality logic if suffix is set
                           )
}

#' Get Brain Mask from Template (DEPRECATED)
#'
#' @description
#' \\strong{DEPRECATED}: Please use \code{\link{get_template}(variant = "mask", ...)} instead.
#'
#' Convenience function to retrieve a binary brain mask for a specified template.
#'
#' @param name Character string specifying template name. Default: "MNI152NLin2009cAsym"
#' @param resolution Numeric resolution in mm. Default: 1
#' @param extension Character string specifying file extension. Default: ".nii.gz"
#' @return A NeuroVol object containing the binary brain mask
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @export
get_template_brainmask <- function(name="MNI152NLin2009cAsym", resolution=1, 
                                  extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0", 
    what = "get_template_brainmask()",
    with = "get_template(variant = \"mask\")"
  )
  neuroatlas::get_template(space = name, variant = "mask", resolution = resolution, 
                           extension = extension)
}

#' Get Tissue Probability Map from Template (DEPRECATED)
#'
#' @description
#' \\strong{DEPRECATED}: Please use \code{\link{get_template}(variant = "probseg", label = ..., ...)} instead.
#'
#' Retrieves probability maps for different tissue types (GM, WM, CSF).
#'
#' @inheritParams get_template_brainmask
#' @param label Character string specifying tissue type ("GM", "WM", or "CSF"). Default: "GM"
#' @return A NeuroVol object containing the probability map
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @export
get_template_probseg <- function(name="MNI152NLin2009cAsym", label="GM", 
                                resolution=1, extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0", 
    what = "get_template_probseg()",
    with = "get_template(variant = \"probseg\", label = ...)"
  )
  neuroatlas::get_template(space = name, variant = "probseg", label = label, 
                           resolution = resolution, extension = extension)
}

#' Get Schaefer Parcellation in Template Space (DEPRECATED)
#'
#' @description
#' \\strong{DEPRECATED}: Please use \code{\link{get_template}(atlas = "Schaefer2018", desc = ..., suffix = "dseg", ...)} instead.
#'
#' Retrieves Schaefer cortical parcellation mapped to a specified template space.
#'
#' @inheritParams get_template_brainmask
#' @param parcels Number of parcels (400 default)
#' @param networks Number of networks (17 default)
#' @return A NeuroVol object containing the parcellation
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @export
get_template_schaefer <- function(name="MNI152NLin2009cAsym", resolution=1,
                                 parcels=400, networks=17, extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0", 
    what = "get_template_schaefer()",
    with = "get_template(atlas = \"Schaefer2018\", ...)"
  )
  desc_str <- paste0(parcels, "Parcels", networks, "Networks")
  neuroatlas::get_template(space = name, desc = desc_str, atlas = "Schaefer2018", 
                           suffix = "dseg", resolution = resolution, extension = extension)
}

#' List Available Templates
#'
#' @description
#' Returns a list of all available templates in the Templateflow repository.
#'
#' @return A character vector of available template names
#' @export
#' @keywords internal
templates <- function() {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "templates()",
    with = "tflow_spaces()"
  )
  # Ensure .tflow_env$py_api is initialized if this is called directly
  if (is.null(.tflow_env$py_api)) {
    create_templateflow() # Initialize if not already
  }
  if (is.null(.tflow_env$py_api)) { # Still NULL after trying
      warning("TemplateFlow API not available. Cannot list templates.")
      return(character(0))
  }
  .tflow_env$py_api$templates()
}

#' Get Template Head Image (DEPRECATED)
#'
#' @description
#' \\strong{DEPRECATED}: Please use \code{\link{get_template}(variant = "head", ...)} instead.
#'
#' Convenience function to get the full head (non-brain-extracted) template.
#'
#' @inheritParams get_template_brainmask
#' @return A NeuroVol object containing the head template
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @export
get_template_head <- function(name="MNI152NLin2009cAsym", resolution=1, 
                            extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0", 
    what = "get_template_head()",
    with = "get_template(variant = \"head\")"
  )
  neuroatlas::get_template(space = name, variant = "head", resolution = resolution, 
                           extension = extension)
}

#' Get CSF Probability Map (DEPRECATED)
#'
#' @description
#' \\strong{DEPRECATED}: Please use \code{\link{get_template}(variant = "probseg", label = "CSF", ...)} instead.
#'
#' Convenience function to get CSF probability map.
#'
#' @inheritParams get_template_brainmask
#' @return A NeuroVol object containing the CSF probability map
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @export
get_template_csf <- function(name="MNI152NLin2009cAsym", resolution=1, 
                            extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0", 
    what = "get_template_csf()",
    with = "get_template(variant = \"probseg\", label = \"CSF\")"
  )
  neuroatlas::get_template(space = name, variant = "probseg", label = "CSF", 
                           resolution = resolution, extension = extension)
}

#' Get Gray Matter Probability Map (DEPRECATED)
#'
#' @description
#' \\strong{DEPRECATED}: Please use \code{\link{get_template}(variant = "probseg", label = "GM", ...)} instead.
#'
#' Convenience function to get gray matter probability map.
#'
#' @inheritParams get_template_brainmask
#' @return A NeuroVol object containing the gray matter probability map
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @export
get_template_gm <- function(name="MNI152NLin2009cAsym", resolution=1, 
                           extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0", 
    what = "get_template_gm()",
    with = "get_template(variant = \"probseg\", label = \"GM\")"
  )
  neuroatlas::get_template(space = name, variant = "probseg", label = "GM", 
                           resolution = resolution, extension = extension)
}

#' Get White Matter Probability Map (DEPRECATED)
#'
#' @description
#' \\strong{DEPRECATED}: Please use \code{\link{get_template}(variant = "probseg", label = "WM", ...)} instead.
#'
#' Convenience function to get white matter probability map.
#'
#' @inheritParams get_template_brainmask
#' @return A NeuroVol object containing the white matter probability map
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @export
get_template_wm <- function(name="MNI152NLin2009cAsym", resolution=1, 
                           extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0", 
    what = "get_template_wm()",
    with = "get_template(variant = \"probseg\", label = \"WM\")"
  )
  neuroatlas::get_template(space = name, variant = "probseg", label = "WM", 
                           resolution = resolution, extension = extension)
}

# Typed Helper Functions for TemplateFlow

#' @rdname get_template
#' @param template_id The main TemplateFlow template identifier for the surface
#'        (e.g., "fsLR", "fsaverage"). This is passed as the `space` argument to `get_template`.
#' @param surface_type A character string indicating the type of surface to retrieve.
#'        Common values include: "pial", "white", "inflated", "midthickness", "sphere".
#'        This is passed as the `desc` argument to `get_template`.
#' @param hemi Character string, "L" for left hemisphere or "R" for right hemisphere.
#'        Passed as `hemi` to `get_template`.
#' @param density (Optional) Character string specifying the surface density
#'        (e.g., "32k" for fsLR, "164k" for fsaverage). Passed as `den` to `get_template`.
#' @param resolution (Optional) Character string specifying the resolution, primarily for
#'        fsaverage variants (e.g., "06" for fsaverage6, which is `tpl-fsaverage_res-06...`).
#'        Passed as `resolution` to `get_template`.
#' @param load_as_path Logical, whether to return only the path to the file.
#'        Defaults to `TRUE` as `NeuroVol` objects are not typically used for surface geometry.
#'        If `FALSE`, attempts to load using `as_neurovol` (via `get_template`).
#' @return If `load_as_path` is `TRUE`, a character string (path) or a list of character strings (paths).
#'         If `load_as_path` is `FALSE`, the result of `as_neurovol` (which might be a `NeuroVol`
#'         if `neuroim2::read_vol` supports the format, or could error if not).
#'         Returns `NULL` if no template is found.
#' @export
#' @examples
#' \donttest{
#'   # Get the pial surface for the left hemisphere of fsLR 32k template (as path)
#'   # fslr_pial_L_path <- get_surface_template(template_id = "fsLR", surface_type = "pial",
#'   #                                        hemi = "L", density = "32k")
#'   # print(fslr_pial_L_path)
#'
#'   # Get the white surface for fsaverage6 (res="06", den="41k") right hemisphere
#'   # fsaverage6_white_R_path <- get_surface_template(template_id = "fsaverage",
#'   #                                               surface_type = "white",
#'   #                                               hemi = "R",
#'   #                                               resolution = "06", # for fsaverage6
#'   #                                               density = "41k")   # for fsaverage6
#'   # print(fsaverage6_white_R_path)
#' }
get_surface_template <- function(template_id, surface_type, hemi,
                                 density = NULL, resolution = NULL,
                                 ...,
                                 load_as_path = TRUE) {

  if (!is.character(template_id) || length(template_id) != 1) {
    stop("'template_id' must be a single character string.")
  }
  if (!is.character(surface_type) || length(surface_type) != 1) {
    stop("'surface_type' must be a single character string.")
  }
  if (!hemi %in% c("L", "R")) {
    stop("'hemi' must be either 'L' or 'R'.")
  }

  # Construct the call to the main get_template function
  # We pass hemi, den (from density), and other ... args directly.
  # resolution is also a direct pass-through for get_template.
  call_args <- list(
    space = template_id,
    desc = surface_type,
    suffix = "surf",        # Common suffix for surface geometry files
    extension = ".gii",     # Common extension for Gifti surface files
    resolution = resolution,
    load_as_neurovol = !load_as_path,
    hemi = hemi
    # den will be added from density if not NULL
  )

  if (!is.null(density)) {
    call_args$den <- density
  }
  
  # Combine with other arguments from ...
  # utils::modifyList will give precedence to arguments in the first list (call_args)
  # if there are any name clashes with ellipsis_args. We want ellipsis_args to add new ones.
  ellipsis_args <- list(...)
  final_args <- utils::modifyList(ellipsis_args, call_args) # puts call_args last, so it overwrites
  # we want ellipsis to be able to override if user passes e.g. suffix explicitly
  # so, the order in modifyList matters. The second list's values take precedence for shared names.
  # Let's ensure explicit params here take precedence over ellipsis, then add non-conflicting ellipsis.
  
  # Start with our fixed/derived args
  base_call_args <- list(
    space = template_id,
    desc = surface_type,
    suffix = "surf",
    extension = ".gii",
    load_as_neurovol = !load_as_path,
    hemi = hemi
  )
  
  # Add resolution if provided (it's a named param in get_template)
  if (!is.null(resolution)) {
    base_call_args$resolution <- resolution
  }
  
  # Add density as 'den' (this goes into ... of get_template)
  additional_tf_params <- list()
  if (!is.null(density)) {
    additional_tf_params$den <- density
  }
  
  # Combine ... from this function call with our derived additional_tf_params
  # an argument explicitly in ... will override one from additional_tf_params if names clash
  final_ellipsis_args <- utils::modifyList(additional_tf_params, ellipsis_args)
  
  # Now combine base_call_args with the final_ellipsis_args
  # Explicit arguments in base_call_args should not be overwritten by final_ellipsis_args
  # So, we add final_ellipsis_args to base_call_args, but only if name not already in base_call_args
  for (arg_name in names(final_ellipsis_args)) {
      if (!arg_name %in% names(base_call_args)) {
          base_call_args[[arg_name]] <- final_ellipsis_args[[arg_name]]
      } else {
          # Potentially warn if ... tries to override a core derived parameter like desc, suffix, etc.
          # For now, we assume user knows what they are doing if they pass e.g. desc in ...
          # However, the new get_template structure with explicit variant/modality handles this better.
          # Here, we let ... override for things like 'den' but not core structure.
          # The most robust is just to pass all ... directly to get_template and let it manage.
          # The current structure of get_template is: named args, then ...
          # So, `den` should be passed via `...` to get_surface_template and then to get_template.
      }
  }

  # Simpler: construct arguments for do.call ensuring 'den' and other ... are correctly passed
  final_args_for_get_template <- list(
    space = template_id,
    desc = surface_type,
    suffix = "surf",
    extension = ".gii",
    resolution = resolution, # Explicitly pass, even if NULL, get_template handles it
    load_as_neurovol = !load_as_path,
    hemi = hemi
  )
  if (!is.null(density)) {
    final_args_for_get_template$den <- density # Add 'den' if density is provided
  }
  
  # Merge with any other ... arguments passed to get_surface_template
  # Arguments in final_args_for_get_template take precedence over those in list(...)
  # if there are name clashes, which is desired for core parameters. 
  # For new parameters from ..., they will be added.
  combined_args <- utils::modifyList(list(...), final_args_for_get_template)

  do.call(get_template, combined_args)
}

# Cache Management Functions ----

#' Clear neuroatlas TemplateFlow Cache
#'
#' Removes all files and subdirectories from the `neuroatlas` package's cache 
#' directory used for TemplateFlow downloads. This function also clears the 
#' in-memory memoisation cache for TemplateFlow path lookups.
#' 
#' The TemplateFlow cache directory is typically located within the path returned by
#' `tools::R_user_dir("neuroatlas", "cache")`, in a subdirectory named "templateflow".
#' 
#' @param confirm Logical. If `TRUE` (the default), the function will ask for 
#'   interactive confirmation before deleting files if the session is interactive.
#'   If `FALSE`, or if the session is not interactive, deletion will proceed without confirmation.
#' @return Invisibly returns `TRUE` if the cache was cleared or attempted to be cleared,
#'   and `FALSE` if the operation was aborted by the user during confirmation.
#' @export
#' @examples
#' \dontrun{
#'   # Clear the TemplateFlow cache (will ask for confirmation if interactive)
#'   # clear_templateflow_cache()
#'   
#'   # Clear without confirmation
#'   # clear_templateflow_cache(confirm = FALSE)
#' }
clear_templateflow_cache <- function(confirm = TRUE) {
  tf_cache_dir <- .neuroatlas_cache_dir("templateflow")
  
  proceed <- FALSE
  if (interactive() && confirm) {
    response <- utils::askYesNo(
      paste0("Are you sure you want to delete all files in the neuroatlas TemplateFlow cache (", 
             tf_cache_dir, ") and clear the memoisation cache?"),
      default = FALSE
    )
    if (!is.na(response) && response) {
      proceed <- TRUE
    }
  } else {
    proceed <- TRUE # Non-interactive or confirm=FALSE
  }
  
  if (!proceed) {
    message("Cache clearing aborted by user.")
    return(invisible(FALSE))
  }
  
  if (dir.exists(tf_cache_dir)) {
    message("Deleting contents of: ", tf_cache_dir)
    # List all files and directories, including hidden ones, but not . and ..
    items_to_delete <- list.files(tf_cache_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(items_to_delete) > 0) {
      unlink(items_to_delete, recursive = TRUE, force = TRUE)
      message("Successfully deleted items from disk cache.")
    } else {
      message("Disk cache directory was empty.")
    }
  } else {
    message("Cache directory not found (already clear or never created): ", tf_cache_dir)
  }
  
  # Clear the memoise cache for the specific function
  if (exists(".memoised_fetch_templateflow_path", where = globalenv()) || 
      exists(".memoised_fetch_templateflow_path", where = asNamespace("neuroatlas"))) {
    tryCatch({
      memoise::forget(.memoised_fetch_templateflow_path)
      message("Successfully cleared in-memory memoisation cache for TemplateFlow paths.")
    }, error = function(e) {
      warning("Could not clear memoisation cache: ", e$message)
    })
  } else {
    message("Memoised function not found; skipping memoisation cache clearing.")
  }
  
  invisible(TRUE)
}

#' Show neuroatlas TemplateFlow Cache Path
#'
#' Returns the path to the `neuroatlas` package's cache directory used for 
#' TemplateFlow downloads. This is typically located within the path returned by
#' `tools::R_user_dir("neuroatlas", "cache")`, in a subdirectory named "templateflow".
#'
#' @return A character string representing the path to the TemplateFlow cache directory.
#' @export
#' @examples
#' cat("TemplateFlow cache is at:", show_templateflow_cache_path(), "\n")
show_templateflow_cache_path <- function() {
  .neuroatlas_cache_dir("templateflow")
}

# Discoverability Functions ----

#' List Available TemplateFlow Template Spaces
#'
#' Retrieves a list of all available template space identifiers from the TemplateFlow archive.
#' These identifiers are top-level names like "MNI152NLin2009cAsym", "fsLR", etc.
#'
#' @param pattern (Optional) A character string containing a regular expression 
#'   to filter the template space names. If `NULL` (default), all names are returned.
#' @param api_handle (Optional) An existing `templateflow` S3 object created by 
#'   `create_templateflow()`. If `NULL`, a default one will be initialized.
#' @param ... Additional arguments passed to `grep` if `pattern` is specified 
#'   (e.g., `ignore.case = TRUE`).
#' @return A character vector of available template space names. Returns `NULL` if the 
#'   list cannot be retrieved, with a warning.
#' @export
#' @examples
#' \donttest{
#'   # List all template spaces
#'   # all_spaces <- tflow_spaces()
#'   # print(head(all_spaces))
#'   
#'   # List template spaces containing "MNI"
#'   # mni_spaces <- tflow_spaces(pattern = "MNI")
#'   # print(mni_spaces)
#' }
tflow_spaces <- function(pattern = NULL, api_handle = NULL, ...) {
  if (is.null(api_handle)) {
    tf <- create_templateflow() 
  } else {
    if (!inherits(api_handle, "templateflow")) {
      stop("'api_handle' must be an object of class 'templateflow' from create_templateflow().")
    }
    tf <- api_handle
  }

  if (is.null(tf$api)) {
    warning("TemplateFlow API handle is not initialized. Cannot list template spaces.")
    return(NULL)
  }

  available_templates_py <- NULL
  tryCatch({
    available_templates_py <- tf$api$templates()
  }, error = function(e) {
    warning(paste0("Could not retrieve list of available TemplateFlow template spaces: ", e$message))
    return(NULL)
  })
  
  if (is.null(available_templates_py)) {
    # Should have been caught by tryCatch, but as a safeguard
    return(NULL) 
  }
  
  available_templates_r <- reticulate::py_to_r(available_templates_py)
  
  if (!is.null(pattern)) {
    if (!is.character(pattern) || length(pattern) != 1) {
      stop("'pattern' must be a single character string (regular expression).")
    }
    available_templates_r <- grep(pattern, available_templates_r, value = TRUE, ...)
  }
  
  return(available_templates_r)
}

#' Find TemplateFlow Files Matching Metadata Criteria
#'
#' Retrieves a list of file paths from TemplateFlow that match a given template 
#' space and other optional metadata query parameters.
#' This function calls the Python `templateflow.api.get()` method with 
#' `raise_on_empty=FALSE` to get a list of all matching files.
#'
#' @param space Character string. The primary TemplateFlow identifier for the template 
#'   space (e.g., "MNI152NLin2009cAsym"). This is passed as `template` to the Python API.
#' @param query_args (Optional) A named list of additional query parameters to 
#'   filter the results (e.g., `list(suffix = "T1w", resolution = "1", desc = "brain")`).
#'   These are passed directly as keyword arguments to the Python `templateflow.api.get()`.
#' @param api_handle (Optional) An existing `templateflow` S3 object created by 
#'   `create_templateflow()`. If `NULL`, a default one will be initialized.
#' @return A character vector of file paths matching the query. Returns an empty 
#'   vector if no files match, or `NULL` with a warning if the API call fails.
#' @export
#' @examples
#' \donttest{
#'   # List all T1w files for MNI152NLin2009cAsym template
#'   # mni_t1w_files <- tflow_files("MNI152NLin2009cAsym", 
#'   #                                           query_args = list(suffix = "T1w"))
#'   # print(mni_t1w_files)
#'   
#'   # List all files for the OASIS30ANTs template with desc "brain"
#'   # oasis_brains <- tflow_files("OASIS30ANTs", 
#'   #                                           query_args = list(desc = "brain"))
#'   # print(oasis_brains)
#' }
tflow_files <- function(space, query_args = list(), api_handle = NULL) {
  if (is.null(api_handle)) {
    tf <- create_templateflow() 
  } else {
    if (!inherits(api_handle, "templateflow")) {
      stop("'api_handle' must be an object of class 'templateflow' from create_templateflow().")
    }
    tf <- api_handle
  }

  if (is.null(tf$api)) {
    warning("TemplateFlow API handle is not initialized. Cannot list metadata.")
    return(NULL)
  }
  
  if (!is.character(space) || length(space) != 1) {
    stop("'space' must be a single character string.")
  }
  if (!is.list(query_args)) {
    stop("'query_args' must be a list.")
  }

  # Construct the full query for the Python API
  full_python_query <- query_args
  full_python_query$template <- space
  full_python_query$raise_on_empty <- FALSE # Ensure it returns list, not error on empty
  
  # Ensure query args are sorted for potential future memoisation consistency if applied here
  if (length(full_python_query) > 0) {
      full_python_query <- full_python_query[sort(names(full_python_query))]
  }

  py_path_list_obj <- NULL
  tryCatch({
    py_path_list_obj <- do.call(tf$api$get, full_python_query)
  }, error = function(e) {
    warning(paste0("TemplateFlow API error while listing metadata: ", e$message,
                   "\nQuery: template=", space, ", args=", paste(names(query_args), query_args, sep="=", collapse=", ")))
    return(NULL) # Return NULL on API error
  })

  if (is.null(py_path_list_obj)) {
    # This can happen if the API call itself failed and returned NULL from tryCatch
    return(NULL) 
  }
  
  # Convert Python list of Path objects to R character vector of paths
  r_paths <- character(0)
  if (reticulate::is_py_object(py_path_list_obj) && inherits(py_path_list_obj, "python.builtin.list")) {
    if (length(py_path_list_obj) > 0) {
      r_paths <- sapply(py_path_list_obj, function(py_path) {
        tryCatch(reticulate::py_to_r(py_path$as_posix()), error = function(e) NA_character_)
      })
      r_paths <- r_paths[!is.na(r_paths)] # Remove any that failed conversion
    }
    # If length is 0, r_paths remains character(0), which is correct for no matches
  } else if (reticulate::is_py_object(py_path_list_obj)) {
    # If TF returns a single Path object when only one file matches (even with raise_on_empty=F)
    # This is unlikely given raise_on_empty=F usually ensures a list, but handle defensively.
    single_path <- tryCatch(reticulate::py_to_r(py_path_list_obj$as_posix()), error = function(e) NA_character_)
    if (!is.na(single_path)) r_paths <- single_path
  } 
  # Else: py_path_list_obj might be something unexpected, r_paths remains empty. 
  # Or if TemplateFlow returns R NULL directly for no matches (unlikely with python object)  

  return(r_paths)
}

# Inter-package Integration Helpers ----

#' Resolve Template Input to NeuroVol or NeuroSpace
#'
#' This internal helper function takes a flexible input representing a neuroimaging
#' template and resolves it to either a `neuroim2::NeuroVol` object or a 
#' `neuroim2::NeuroSpace` object, typically by fetching it via `get_template()`
#' if it's not already in the desired R object form.
#'
#' @param input The input to resolve. Can be:
#'   - A `neuroim2::NeuroVol` object.
#'   - A `neuroim2::NeuroSpace` object.
#'   - A character string: Assumed to be a TemplateFlow `space` identifier. 
#'     `get_template()` will be called with this space and default values for 
#'     other parameters (e.g., `variant="brain"`, `resolution="1"`).
#'   - A named list: Assumed to be arguments for `get_template()`.
#'     `do.call(get_template, input)` will be used.
#' @param target_type A character string, either "NeuroVol" (default) or "NeuroSpace",
#'   specifying the desired output type.
#' @param api_handle (Optional) An existing `templateflow` S3 object.
#' @return An object of the `target_type`. If `target_type` is "NeuroSpace" and 
#'   a `NeuroVol` is obtained, its space is extracted via `neuroim2::space()`. 
#'   Returns `NULL` or stops on error if resolution fails.
#' @keywords internal
#' @importFrom neuroim2 space
.resolve_template_input <- function(input, target_type = "NeuroVol", api_handle = NULL) {
  if (!target_type %in% c("NeuroVol", "NeuroSpace")) {
    stop("'target_type' must be either 'NeuroVol' or 'NeuroSpace'.")
  }

  resolved_vol <- NULL

  if (inherits(input, "NeuroVol")) {
    resolved_vol <- input
  } else if (inherits(input, "NeuroSpace")) {
    if (target_type == "NeuroSpace") {
      return(input) # Already correct type
    } else { # target_type == "NeuroVol"
      stop("Input is a NeuroSpace, but target_type is NeuroVol. Cannot convert NeuroSpace to NeuroVol without more information.")
    }
  } else if (is.character(input) && length(input) == 1) {
    # Assume string is a TemplateFlow space ID, use default params for get_template
    message("Resolving template input string '", input, "' as a TemplateFlow space ID with default parameters.")
    resolved_vol <- tryCatch({
      get_template(space = input, api_handle = api_handle) # Uses defaults of get_template
    }, error = function(e) {
      stop("Failed to resolve template string '", input, "' via get_template(): ", conditionMessage(e))
      return(NULL) # Should be caught by stop
    })
  } else if (is.list(input)) {
    # Assume list is arguments for get_template
    message("Resolving template input list via do.call(get_template, ...).")
    if (!is.null(api_handle)) {
      input$api_handle <- api_handle # Add api_handle if provided
    }
    resolved_vol <- tryCatch({
      do.call(get_template, input)
    }, error = function(e) {
      stop("Failed to resolve template list via get_template(): ", conditionMessage(e))
      return(NULL) # Should be caught by stop
    })
  } else {
    stop("Invalid 'input' type. Must be a NeuroVol, NeuroSpace, template name (string), or list of get_template() arguments.")
  }

  if (is.null(resolved_vol)) {
    # This case should ideally be handled by errors within the conditional blocks
    stop("Failed to obtain a NeuroVol from the provided input.")
  }
  
  if (!inherits(resolved_vol, "NeuroVol")){
      stop("Resolution of input did not result in a NeuroVol object as expected.")
  }

  # Now, convert to target_type if necessary
  if (target_type == "NeuroVol") {
    return(resolved_vol)
  } else { # target_type == "NeuroSpace"
    return(neuroim2::space(resolved_vol))
  }
}

# Typed Helper Functions for TemplateFlow