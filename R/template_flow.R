#' @importFrom tools R_user_dir
#' @importFrom memoise memoise
#' @importFrom lifecycle deprecate_warn
#' @importFrom utils modifyList head askYesNo
NULL

# ---- Internal Helpers ----

#' Check that the templateflow R package is available
#' @keywords internal
#' @noRd
.ensure_templateflow <- function() {
  if (!requireNamespace("templateflow", quietly = TRUE)) {
    stop(
      "The 'templateflow' R package is required for TemplateFlow features.\n",
      "Install it with: remotes::install_github('bbuchsbaum/templateflow')",
      call. = FALSE
    )
  }
}

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

#' Check TemplateFlow Connectivity
#'
#' Internal function to test if TemplateFlow API is accessible and functioning.
#'
#' @return Logical. TRUE if TemplateFlow API is accessible, FALSE otherwise.
#' @keywords internal
.check_templateflow_connectivity <- function() {
  if (!requireNamespace("templateflow", quietly = TRUE)) {
    return(FALSE)
  }
  tryCatch({
    spaces <- templateflow::tf_templates()
    length(spaces) > 0
  }, error = function(e) {
    message("TemplateFlow appears to be unreachable: ", e$message)
    FALSE
  })
}

# ---- TemplateFlow S3 Object (Deprecated) ----

#' Create a TemplateFlow Interface Object
#'
#' @description
#' **DEPRECATED:** TemplateFlow is now accessed via the pure R `templateflow`
#' package. No initialization is needed. Use [get_template()], [tflow_spaces()],
#' and [tflow_files()] directly.
#'
#' @param cache_dir Ignored. Kept for backward compatibility.
#' @param verbosity Ignored. Kept for backward compatibility.
#' @param default_template Ignored. Kept for backward compatibility.
#'
#' @return An S3 object of class \code{templateflow} (deprecated stub).
#' @export
#' @examples
#' \dontrun{
#'   # Deprecated. Use get_template(), tflow_spaces(), etc. directly.
#' }
create_templateflow <- function(cache_dir = NULL, verbosity = 0, default_template = NULL) {
  lifecycle::deprecate_warn(
    "0.2.0", "create_templateflow()",
    details = paste(
      "TemplateFlow is now accessed via the pure R 'templateflow' package.",
      "No initialization is needed.",
      "Use get_template(), tflow_spaces(), and tflow_files() directly."
    )
  )
  .ensure_templateflow()
  obj <- list(
    cache_path = if (!is.null(cache_dir)) cache_dir else .neuroatlas_cache_dir("templateflow"),
    options = list(verbosity = verbosity, default_template = default_template)
  )
  class(obj) <- "templateflow"
  obj
}

#' Print a TemplateFlow Object
#'
#' Provides a brief summary of the TemplateFlow interface object.
#'
#' @param x An object of class \code{templateflow}.
#' @param ... Additional arguments (unused).
#' @return The input object \code{x}, returned invisibly.
#' @export
print.templateflow <- function(x, ...) {
  cat("<neuroatlas TemplateFlow Interface (R backend)>\n")
  cat("  Cache Path: ", x[["cache_path"]], "\n")
  tryCatch({
    .ensure_templateflow()
    spaces <- templateflow::tf_templates()
    cat("  Available Templates (Examples): ",
        paste(utils::head(spaces, 5), collapse = ", "))
    if (length(spaces) > 5) {
      cat(", ... (Total: ", length(spaces), ")\n", sep = "")
    } else {
      cat(" (Total: ", length(spaces), ")\n", sep = "")
    }
  }, error = function(e) {
    cat("  Available Templates: Error retrieving list - ", e$message, "\n")
  })
  invisible(x)
}

#' Access Attributes of the TemplateFlow Object (DEPRECATED)
#'
#' @param x An object of class \code{templateflow}.
#' @param name The name of the attribute to access.
#' @return The list element, or NULL if not found.
#' @export
#' @method $ templateflow
#' @keywords internal
`$.templateflow` <- function(x, name) {
  unclass(x)[[name]]
}

#' Access Attributes of the TemplateFlow Object using [[ (DEPRECATED)
#'
#' @param x An object of class \code{templateflow}.
#' @param name The name of the attribute to access.
#' @return The list element, or NULL if not found.
#' @export
#' @method [[ templateflow
#' @keywords internal
`[[.templateflow` <- function(x, name) {
  unclass(x)[[name]]
}

#' List Attributes of the TemplateFlow Object (DEPRECATED)
#'
#' @param x An object of class \code{templateflow}.
#' @return A character vector of available element names.
#' @export
#' @method names templateflow
#' @keywords internal
names.templateflow <- function(x) {
  names(unclass(x))
}

# ---- Main Template Retrieval ----

#' Fetch a Template from TemplateFlow
#'
#' Unified function to retrieve neuroimaging templates and related files from
#' the TemplateFlow repository via the pure R \code{templateflow} package.
#'
#' @param space Character string. The primary TemplateFlow identifier for the template space
#'   (e.g., \code{"MNI152NLin2009cAsym"}). Default: \code{"MNI152NLin2009cAsym"}.
#' @param variant Character string. A high-level descriptor for common template types.
#'   Supported: \code{"brain"} (default), \code{"head"}, \code{"mask"}, \code{"probseg"}, \code{"dseg"}.
#'   This is used to infer \code{desc} and sometimes \code{suffix} if they are not explicitly provided.
#' @param modality Character string. The imaging modality or primary suffix for the template file.
#'   Supported: \code{"T1w"} (default), \code{"T2w"}, \code{"mask"}.
#'   This is used to infer \code{suffix} if not explicitly provided.
#' @param resolution Numeric or character. The resolution of the template in mm (e.g., \code{1}, \code{2}). Default: \code{1}.
#' @param cohort Character string. Optional cohort identifier.
#' @param desc Character string. Specific TemplateFlow \code{desc} field. Defaults to \code{"brain"}.
#' @param label Character string. Specific TemplateFlow \code{label} field (e.g., \code{"GM"}, \code{"WM"}, \code{"CSF"}).
#' @param atlas Character string. Specific TemplateFlow \code{atlas} field (e.g., \code{"Schaefer2018"}).
#' @param suffix Character string. Specific TemplateFlow \code{suffix} field. Overrides
#'   any \code{suffix} inferred from \code{modality} or \code{variant}.
#' @param extension Character string. The file extension. Default: \code{".nii.gz"}.
#' @param path_only Logical. If \code{TRUE}, returns the file path as a string
#'   instead of loading as a \code{NeuroVol}. Default: \code{FALSE}.
#' @param use_cache Logical. If \code{TRUE} (default), uses memoised NeuroVol loading.
#' @param api_handle Deprecated and ignored. Kept for backward compatibility.
#' @param ... Additional arguments passed to the TemplateFlow query
#'   (e.g., \code{hemi}, \code{density}).
#'
#' @details
#' The function performs several pre-flight checks:
#'   - Validates the existence of the specified \code{space}.
#'   - Validates the specified \code{resolution} against available resolutions.
#'
#' Caching behaviour:
#'   - The \code{templateflow} R package maintains its own disk cache.
#'   - \code{NeuroVol} loading is memoised at the R session level to avoid
#'     re-reading large NIfTI files.
#'
#' @return If any of \code{space}, \code{variant}, \code{modality}, \code{resolution},
#'   or \code{label} are vectors of length > 1, a named list of results is returned.
#'   Otherwise a single \code{neuroim2::NeuroVol} or file path string.
#'
#' @importFrom neuroim2 NeuroVol
#' @export
#' @examples
#' \dontrun{
#'   # Get default MNI T1w brain template
#'   mni_brain <- get_template()
#'
#'   # Vectorized: Get MNI brain and mask variants
#'   mni_variants <- get_template(variant = c("brain", "mask"))
#'
#'   # Path only
#'   path <- get_template(path_only = TRUE)
#' }
get_template <- function(space = "MNI152NLin2009cAsym",
                         variant = "brain",
                         modality = "T1w",
                         resolution = 1,
                         cohort = NULL,
                         desc = "brain",
                         label = NULL,
                         atlas = NULL,
                         suffix = NULL,
                         extension = ".nii.gz",
                         path_only = FALSE,
                         use_cache = TRUE,
                         api_handle = NULL,
                         ...) {

  desc_missing <- missing(desc)

  # --- Vectorized Argument Handling ---
  vectorizable_params <- list(
    space = space,
    variant = variant,
    modality = modality,
    resolution = resolution,
    label = label
  )
  vector_lengths <- sapply(vectorizable_params, length)
  is_vectorized <- vector_lengths > 1
  num_vectorized <- sum(is_vectorized)

  if (num_vectorized > 1) {
    stop("Vectorization is supported for only one parameter at a time. ",
         "Use lapply() or purrr::map() for multiple parameters.")
  }

  if (num_vectorized == 1) {
    vec_param_name <- names(is_vectorized)[is_vectorized]
    vec_values <- vectorizable_params[[vec_param_name]]

    # Capture extra arguments
    extra_args <- list(...)

    # Build results list by calling scalar version
    results_list <- lapply(vec_values, function(val) {
      args <- list(
        space = if (vec_param_name == "space") val else space[1],
        variant = if (vec_param_name == "variant") val else variant[1],
        modality = if (vec_param_name == "modality") val else modality[1],
        resolution = if (vec_param_name == "resolution") val else resolution[1],
        label = if (vec_param_name == "label") val else label[1],
        cohort = cohort,
        desc = desc,
        desc_missing = desc_missing,
        atlas = atlas,
        suffix = suffix,
        extension = extension,
        path_only = path_only,
        use_cache = use_cache,
        api_handle = api_handle
      )
      args <- c(args, extra_args)
      do.call(.get_template_scalar, args)
    })

    names(results_list) <- as.character(vec_values)
    return(results_list)
  }

  # --- No vectorization - proceed with scalar implementation ---
  .get_template_scalar(
    space = space,
    variant = variant,
    modality = modality,
    resolution = resolution,
    label = label,
    cohort = cohort,
    desc = desc,
    desc_missing = desc_missing,
    atlas = atlas,
    suffix = suffix,
    extension = extension,
    path_only = path_only,
    use_cache = use_cache,
    api_handle = api_handle,
    ...
  )
}

# Internal scalar implementation
.get_template_scalar <- function(space, variant, modality, resolution,
                                label, cohort, desc, atlas, suffix,
                                extension, path_only, use_cache,
                                api_handle, desc_missing = FALSE, ...) {
  .ensure_templateflow()

  # Validate template space
  .validate_template_space(space)

  # Validate resolution if provided
  if (!is.null(resolution)) {
    .validate_resolution(space, resolution)
  }

  # Infer parameters
  params <- .infer_template_params(
    variant = variant,
    modality = modality,
    desc = desc,
    suffix = suffix,
    label = label,
    desc_missing = desc_missing
  )

  # Build query arguments
  query_args <- .build_query_args(
    space = space,
    resolution = resolution,
    desc = params$desc,
    suffix = params$suffix,
    label = label,
    atlas = atlas,
    cohort = cohort,
    extension = extension,
    ...
  )

  # Fetch template path via R templateflow package
  file_path <- tryCatch({
    do.call(templateflow::tf_get, query_args)
  }, error = function(e) {
    stop(structure(
      list(message = paste0("TemplateFlow error: ", conditionMessage(e),
                            "\nQuery args: ",
                            paste(names(query_args), query_args,
                                  sep = "=", collapse = ", ")),
           call = NULL,
           query_args = query_args),
      class = c("templateflow_api_error", "error", "condition")
    ))
  })

  if (length(file_path) == 0 ||
      (length(file_path) == 1 && !nzchar(file_path))) {
    stop(structure(
      list(message = paste0("TemplateFlow found no files for the given query.",
                            "\nQuery args: ",
                            paste(names(query_args), query_args,
                                  sep = "=", collapse = ", ")),
           call = NULL,
           query_args = query_args),
      class = c("templateflow_no_files_error", "error", "condition")
    ))
  }

  if (length(file_path) > 1) {
    warning("TemplateFlow returned multiple files (",
            length(file_path), ") for the query, using the first one: ",
            file_path[1], call. = FALSE)
    file_path <- file_path[1]
  }

  if (path_only) {
    return(file_path)
  } else {
    return(as_neurovol(file_path))
  }
}

# ---- NeuroVol Loading ----

#' Internal Helper to Convert File Path to NeuroVol
#'
#' Reads a NIfTI file into a \code{neuroim2::NeuroVol} object.
#'
#' @param file_path A character string file path to a NIfTI file.
#' @return A \code{neuroim2::NeuroVol} object.
#' @importFrom neuroim2 read_vol
#' @keywords internal
.as_neurovol_unmemoised <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("'file_path' must be a single character string.")
  }
  if (!file.exists(file_path)) {
    stop("Resolved file path does not exist: ", file_path)
  }
  tryCatch({
    neuroim2::read_vol(file_path)
  }, error = function(e) {
    stop("Failed to read NIfTI file (", file_path, ") into NeuroVol: ", e$message)
  })
}

#' Memoised version of .as_neurovol_unmemoised
#' @keywords internal
#' @noRd
as_neurovol <- memoise::memoise(.as_neurovol_unmemoised)

# ---- Legacy Wrapper Functions ----

#' Access Templateflow Brain Templates (DEPRECATED - Legacy Signature)
#'
#' @description
#' **DEPRECATED:** This function signature is deprecated. Please use the new
#' \code{\link{get_template}} function which offers a more comprehensive
#' and R-native interface.
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
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch(result <- get_template_legacy(), error = function(e) NULL)
#' }
#' }
#' @export
#' @rdname get_template_legacy
get_template_legacy <- function(name="MNI152NLin2009cAsym", desc="brain", resolution=1,
                               label=NULL, atlas=NULL, suffix="T1w",
                               extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "get_template(name)",
    with = "get_template(space)",
    details = paste0(
      "The signature get_template(name, desc, resolution, ...) is deprecated.\n",
      "Please use the new signature: get_template(space, variant, modality, resolution, ...)."
    )
  )
  neuroatlas::get_template(space = name,
                           desc = desc,
                           resolution = resolution,
                           label = label,
                           atlas = atlas,
                           suffix = suffix,
                           extension = extension,
                           variant = NULL,
                           modality = NULL)
}

#' Get Brain Mask from Template (DEPRECATED)
#'
#' @description
#' **DEPRECATED:** Please use \code{\link{get_template}(variant = "mask", ...)} instead.
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
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch(result <- get_template_brainmask(), error = function(e) NULL)
#' }
#' }
#' @export
get_template_brainmask <- function(name="MNI152NLin2009cAsym", resolution=1,
                                  extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "get_template_brainmask(name)",
    with = "get_template()"
  )
  neuroatlas::get_template(space = name, variant = "mask", resolution = resolution,
                           extension = extension)
}

#' Get Tissue Probability Map from Template (DEPRECATED)
#'
#' @description
#' **DEPRECATED:** Please use \code{\link{get_template}(variant = "probseg", label = ..., ...)} instead.
#'
#' Retrieves probability maps for different tissue types (GM, WM, CSF).
#'
#' @inheritParams get_template_brainmask
#' @param label Character string specifying tissue type ("GM", "WM", or "CSF"). Default: "GM"
#' @return A NeuroVol object containing the probability map
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch(result <- get_template_probseg(), error = function(e) NULL)
#' }
#' }
#' @export
get_template_probseg <- function(name="MNI152NLin2009cAsym", label="GM",
                                resolution=1, extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "get_template_probseg(name)",
    with = "get_template()"
  )
  neuroatlas::get_template(space = name, variant = "probseg", label = label,
                           resolution = resolution, extension = extension)
}

#' Get Schaefer Parcellation in Template Space (DEPRECATED)
#'
#' @description
#' **DEPRECATED:** Please use \code{\link{get_template}(atlas = "Schaefer2018", desc = ..., suffix = "dseg", ...)} instead.
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
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch(result <- get_template_schaefer(), error = function(e) NULL)
#' }
#' }
#' @export
get_template_schaefer <- function(name="MNI152NLin2009cAsym", resolution=1,
                                 parcels=400, networks=17, extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "get_template_schaefer(name)",
    with = "get_template()"
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
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch({available <- templates(); head(available)},
#'     error = function(e) NULL)
#' }
#' }
#' @export
#' @keywords internal
templates <- function() {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "templates()",
    with = "tflow_spaces()"
  )
  tflow_spaces()
}

#' Get Template Head Image (DEPRECATED)
#'
#' @description
#' **DEPRECATED:** Please use \code{\link{get_template}(variant = "head", ...)} instead.
#'
#' Convenience function to get the full head (non-brain-extracted) template.
#'
#' @inheritParams get_template_brainmask
#' @return A NeuroVol object containing the head template
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch(result <- get_template_head(), error = function(e) NULL)
#' }
#' }
#' @export
get_template_head <- function(name="MNI152NLin2009cAsym", resolution=1,
                            extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "get_template_head(name)",
    with = "get_template()"
  )
  neuroatlas::get_template(space = name, variant = "head", resolution = resolution,
                           extension = extension)
}

#' Get CSF Probability Map (DEPRECATED)
#'
#' @description
#' **DEPRECATED:** Please use \code{\link{get_template}(variant = "probseg", label = "CSF", ...)} instead.
#'
#' Convenience function to get CSF probability map.
#'
#' @inheritParams get_template_brainmask
#' @return A NeuroVol object containing the CSF probability map
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch(result <- get_template_csf(), error = function(e) NULL)
#' }
#' }
#' @export
get_template_csf <- function(name="MNI152NLin2009cAsym", resolution=1,
                            extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "get_template_csf(name)",
    with = "get_template()"
  )
  neuroatlas::get_template(space = name, variant = "probseg", label = "CSF",
                           resolution = resolution, extension = extension)
}

#' Get Gray Matter Probability Map (DEPRECATED)
#'
#' @description
#' **DEPRECATED:** Please use \code{\link{get_template}(variant = "probseg", label = "GM", ...)} instead.
#'
#' Convenience function to get gray matter probability map.
#'
#' @inheritParams get_template_brainmask
#' @return A NeuroVol object containing the gray matter probability map
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch(result <- get_template_gm(), error = function(e) NULL)
#' }
#' }
#' @export
get_template_gm <- function(name="MNI152NLin2009cAsym", resolution=1,
                           extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "get_template_gm(name)",
    with = "get_template()"
  )
  neuroatlas::get_template(space = name, variant = "probseg", label = "GM",
                           resolution = resolution, extension = extension)
}

#' Get White Matter Probability Map (DEPRECATED)
#'
#' @description
#' **DEPRECATED:** Please use \code{\link{get_template}(variant = "probseg", label = "WM", ...)} instead.
#'
#' Convenience function to get white matter probability map.
#'
#' @inheritParams get_template_brainmask
#' @return A NeuroVol object containing the white matter probability map
#' @seealso The new \code{\link{get_template}}
#' @md
#' @keywords internal
#' @examples
#' \donttest{
#' if (requireNamespace("templateflow", quietly = TRUE)) {
#'   tryCatch(result <- get_template_wm(), error = function(e) NULL)
#' }
#' }
#' @export
get_template_wm <- function(name="MNI152NLin2009cAsym", resolution=1,
                           extension=".nii.gz") {
  lifecycle::deprecate_warn(
    when = "0.10.0",
    what = "get_template_wm(name)",
    with = "get_template()"
  )
  neuroatlas::get_template(space = name, variant = "probseg", label = "WM",
                           resolution = resolution, extension = extension)
}

# ---- Surface Template Functions ----

#' @rdname get_template
#' @param template_id The main TemplateFlow template identifier for the surface
#'        (e.g., "fsLR", "fsaverage"). This is passed as the `space` argument to `get_template`.
#' @param surface_type A character string indicating the type of surface to retrieve.
#'        Common values include: "pial", "white", "inflated", "midthickness", "sphere".
#'        This is passed as the `suffix` argument to `get_template`.
#' @param hemi Character string, "L" for left hemisphere or "R" for right hemisphere.
#'        Passed as `hemi` to `get_template`.
#' @param density (Optional) Character string specifying the surface density
#'        (e.g., "32k" for fsLR, "164k" for fsaverage). Forwarded to TemplateFlow
#'        as `density`.
#' @param resolution (Optional) Character string specifying the resolution, primarily for
#'        fsaverage variants (e.g., "06" for fsaverage6).
#' @param load_as_path Logical, whether to return only the path to the file.
#'        Defaults to `TRUE`.
#' @return If `load_as_path` is `TRUE`, a character string (path).
#'         If `load_as_path` is `FALSE`, the result of `as_neurovol`.
#' @export
#' @examples
#' \donttest{
#'   # Get the pial surface for the left hemisphere of fsLR 32k template (as path)
#'   # fslr_pial_L_path <- get_surface_template(template_id = "fsLR", surface_type = "pial",
#'   #                                        hemi = "L", density = "32k")
#'   # print(fslr_pial_L_path)
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

  final_args_for_get_template <- list(
    space = template_id,
    suffix = surface_type,
    extension = ".surf.gii",
    hemi = hemi,
    path_only = load_as_path
  )

  if (!is.null(resolution)) {
    final_args_for_get_template$resolution <- resolution
  }
  if (!is.null(density)) {
    final_args_for_get_template$density <- density
  }

  # Merge with any other ... arguments
  combined_args <- utils::modifyList(list(...), final_args_for_get_template)

  # Surface templates should not use volumetric defaults
  if (is.null(resolution)) {
    combined_args["resolution"] <- list(NULL)
  }
  combined_args["variant"] <- list(NULL)
  combined_args["modality"] <- list(NULL)
  combined_args["desc"] <- list(NULL)

  do.call(get_template, combined_args)
}

#' Load a surface template as a neurosurf geometry
#'
#' Convenience wrapper around \code{\link{get_surface_template}} that
#' downloads (via TemplateFlow) the requested surface geometry and returns it
#' as a \code{neurosurf::SurfaceGeometry} object (or a left/right list).
#'
#' @param template_id Surface template identifier passed to TemplateFlow
#'   (e.g., "fsaverage", "fsaverage6", "fsLR").
#' @param surface_type Surface type (e.g., "white", "pial", "inflated",
#'   "midthickness").
#' @param hemi Hemisphere to load. One of "L", "R", or "both". If "both",
#'   a named list with elements \code{L} and \code{R} is returned.
#' @param density Optional surface density (TemplateFlow \code{density}
#'   argument).
#' @param resolution Optional resolution string (TemplateFlow \code{res}
#'   argument), e.g., "06" for fsaverage6.
#' @param ... Additional arguments forwarded to \code{\link{get_surface_template}}.
#'
#' @return A \code{neurosurf::SurfaceGeometry} object when \code{hemi} is "L"
#'   or "R"; a named list of two \code{SurfaceGeometry} objects when
#'   \code{hemi} is "both".
#'
#' @examples
#' \dontrun{
#'   # fsaverage6 pial surface as NeuroSurface
#'   lh <- load_surface_template("fsaverage", "pial", hemi = "L",
#'                               density = "41k", resolution = "06")
#'
#'   # Both hemispheres of fsLR 32k inflated surface
#'   both <- load_surface_template("fsLR", "inflated", hemi = "both",
#'                                 density = "32k")
#' }
#' @export
load_surface_template <- function(template_id, surface_type,
                                  hemi = c("L", "R", "both"),
                                  density = NULL,
                                  resolution = NULL,
                                  ...) {
  hemi <- match.arg(hemi)

  fetch_one <- function(h) {
    surf_path <- get_surface_template(
      template_id = template_id,
      surface_type = surface_type,
      hemi = h,
      density = density,
      resolution = resolution,
      ...,
      load_as_path = TRUE
    )

    neurosurf::read_surf_geometry(surf_path)
  }

  if (identical(hemi, "both")) {
    ret <- list(L = fetch_one("L"), R = fetch_one("R"))
    class(ret) <- c("neuroatlas_surface_pair", "list")
    return(ret)
  }

  fetch_one(hemi)
}

# ---- Cache Management Functions ----

#' Clear neuroatlas TemplateFlow Cache
#'
#' Removes cached files and clears in-memory memoisation.
#'
#' @param confirm Logical. If `TRUE` (the default), asks for interactive confirmation.
#' @return Invisibly returns `TRUE` if the cache was cleared, `FALSE` if aborted.
#' @export
#' @examples
#' \dontrun{
#'   clear_templateflow_cache()
#'   clear_templateflow_cache(confirm = FALSE)
#' }
clear_templateflow_cache <- function(confirm = TRUE) {
  proceed <- FALSE
  if (interactive() && confirm) {
    response <- utils::askYesNo(
      "Are you sure you want to clear the TemplateFlow cache and memoisation?",
      default = FALSE
    )
    if (!is.na(response) && response) {
      proceed <- TRUE
    }
  } else {
    proceed <- TRUE
  }

  if (!proceed) {
    message("Cache clearing aborted by user.")
    return(invisible(FALSE))
  }

  # Clear the templateflow R package cache if available
  if (requireNamespace("templateflow", quietly = TRUE)) {
    tryCatch({
      templateflow::tf_cache_wipe()
      message("Successfully cleared templateflow disk cache.")
    }, error = function(e) {
      message("Could not clear templateflow cache: ", e$message)
    })
  }

  # Also clear the neuroatlas-specific cache dir (legacy)
  tf_cache_dir <- .neuroatlas_cache_dir("templateflow")
  if (dir.exists(tf_cache_dir)) {
    items_to_delete <- list.files(tf_cache_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(items_to_delete) > 0) {
      unlink(items_to_delete, recursive = TRUE, force = TRUE)
      message("Cleared legacy neuroatlas cache: ", tf_cache_dir)
    }
  }

  # Clear NeuroVol memoisation
  tryCatch({
    memoise::forget(as_neurovol)
    message("Cleared in-memory memoisation cache for NeuroVol loading.")
  }, error = function(e) {
    # Ignore - memoise cache may not exist yet
  })

  invisible(TRUE)
}

#' Show neuroatlas TemplateFlow Cache Path
#'
#' Returns the path to the TemplateFlow cache directory. With the pure R
#' \code{templateflow} package, this is the \code{TEMPLATEFLOW_HOME} directory.
#'
#' @return A character string representing the path to the TemplateFlow cache directory.
#' @export
#' @examples
#' cat("TemplateFlow cache is at:", show_templateflow_cache_path(), "\n")
show_templateflow_cache_path <- function() {
  if (requireNamespace("templateflow", quietly = TRUE)) {
    tryCatch(
      return(templateflow::tf_home()),
      error = function(e) NULL
    )
  }
  # Fallback to legacy path
  .neuroatlas_cache_dir("templateflow")
}

# ---- Discoverability Functions ----

#' List Available TemplateFlow Template Spaces
#'
#' Retrieves a list of all available template space identifiers from the TemplateFlow archive.
#'
#' @param pattern (Optional) A character string containing a regular expression
#'   to filter the template space names. If `NULL` (default), all names are returned.
#' @param api_handle Deprecated and ignored.
#' @param ... Additional arguments passed to `grep` if `pattern` is specified
#'   (e.g., `ignore.case = TRUE`).
#' @return A character vector of available template space names.
#' @export
#' @examples
#' \donttest{
#'   # List all template spaces
#'   # all_spaces <- tflow_spaces()
#'
#'   # List template spaces containing "MNI"
#'   # mni_spaces <- tflow_spaces(pattern = "MNI")
#' }
tflow_spaces <- function(pattern = NULL, api_handle = NULL, ...) {
  .ensure_templateflow()

  available <- tryCatch({
    templateflow::tf_templates()
  }, error = function(e) {
    warning("Could not retrieve available TemplateFlow template spaces: ", e$message)
    return(NULL)
  })

  if (is.null(available)) {
    return(NULL)
  }

  if (!is.null(pattern)) {
    if (!is.character(pattern) || length(pattern) != 1) {
      stop("'pattern' must be a single character string (regular expression).")
    }
    available <- grep(pattern, available, value = TRUE, ...)
  }

  return(available)
}

#' Find TemplateFlow Files Matching Metadata Criteria
#'
#' Retrieves a list of file paths from TemplateFlow that match a given template
#' space and other optional metadata query parameters.
#'
#' @param space Character string. The TemplateFlow identifier for the template
#'   space (e.g., "MNI152NLin2009cAsym").
#' @param query_args (Optional) A named list of additional query parameters to
#'   filter the results (e.g., `list(suffix = "T1w", resolution = 1, desc = "brain")`).
#' @param api_handle Deprecated and ignored.
#' @return A character vector of file paths matching the query. Returns an empty
#'   vector if no files match.
#' @export
#' @examples
#' \donttest{
#'   # List all T1w files for MNI152NLin2009cAsym template
#'   # mni_t1w_files <- tflow_files("MNI152NLin2009cAsym",
#'   #                              query_args = list(suffix = "T1w"))
#' }
tflow_files <- function(space, query_args = list(), api_handle = NULL) {
  .ensure_templateflow()

  if (!is.character(space) || length(space) != 1) {
    stop("'space' must be a single character string.")
  }
  if (!is.list(query_args)) {
    stop("'query_args' must be a list.")
  }

  # Build the arguments for tf_ls
  args <- c(list(template = space), query_args)

  tryCatch({
    do.call(templateflow::tf_ls, args)
  }, error = function(e) {
    warning("TemplateFlow query error: ", e$message)
    character(0)
  })
}

# ---- Inter-package Integration Helpers ----

#' Resolve Template Input to NeuroVol or NeuroSpace
#'
#' Internal helper function that takes a flexible input representing a neuroimaging
#' template and resolves it to either a \code{NeuroVol} or \code{NeuroSpace} object.
#'
#' @param input The input to resolve. Can be a \code{NeuroVol}, \code{NeuroSpace},
#'   a TemplateFlow space string, or a named list of \code{get_template()} arguments.
#' @param target_type "NeuroVol" (default) or "NeuroSpace".
#' @param api_handle Deprecated and ignored.
#' @return An object of the specified \code{target_type}.
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
      return(input)
    } else {
      stop("Input is a NeuroSpace, but target_type is NeuroVol. ",
           "Cannot convert NeuroSpace to NeuroVol without more information.")
    }
  } else if (is.character(input) && length(input) == 1) {
    message("Resolving template input string '", input,
            "' as a TemplateFlow space ID with default parameters.")
    resolved_vol <- tryCatch({
      get_template(space = input)
    }, error = function(e) {
      stop("Failed to resolve template string '", input,
           "' via get_template(): ", conditionMessage(e))
    })
  } else if (is.list(input)) {
    message("Resolving template input list via do.call(get_template, ...).")
    resolved_vol <- tryCatch({
      do.call(get_template, input)
    }, error = function(e) {
      stop("Failed to resolve template list via get_template(): ",
           conditionMessage(e))
    })
  } else {
    stop("Invalid 'input' type. Must be a NeuroVol, NeuroSpace, ",
         "template name (string), or list of get_template() arguments.")
  }

  if (is.null(resolved_vol)) {
    stop("Failed to obtain a NeuroVol from the provided input.")
  }

  if (!inherits(resolved_vol, "NeuroVol")) {
    stop("Resolution of input did not result in a NeuroVol object as expected.")
  }

  if (target_type == "NeuroVol") {
    return(resolved_vol)
  } else {
    return(neuroim2::space(resolved_vol))
  }
}

# ---- Validation Helpers ----

# Helper: Validate template space
.validate_template_space <- function(space) {
  .ensure_templateflow()
  available <- tryCatch({
    templateflow::tf_templates()
  }, error = function(e) {
    warning("Could not retrieve available templates: ", e$message)
    return(NULL)
  })

  if (!is.null(available) && !(space %in% available)) {
    stop("Template space '", space, "' not found. ",
         "Available: ", paste(available, collapse = ", "))
  }
}

# Helper: Validate resolution
.validate_resolution <- function(space, resolution) {
  .ensure_templateflow()

  # Check if any files exist for this space+resolution combo
  files <- tryCatch({
    templateflow::tf_ls(template = space, resolution = as.integer(resolution))
  }, error = function(e) NULL)

  if (!is.null(files) && length(files) == 0) {
    # Try to find what resolutions ARE available
    all_files <- tryCatch({
      templateflow::tf_ls(template = space, as_df = TRUE)
    }, error = function(e) NULL)

    if (!is.null(all_files) && is.data.frame(all_files) &&
        nrow(all_files) > 0 && "resolution" %in% names(all_files)) {
      avail_res <- sort(unique(all_files$resolution[!is.na(all_files$resolution)]))
      if (length(avail_res) > 0) {
        stop("Resolution '", resolution, "' not available for ", space, ". ",
             "Available: ", paste(avail_res, collapse = ", "))
      }
    }
  }

  invisible(TRUE)
}

# ---- Parameter Inference Helpers ----

.infer_template_params <- function(variant, modality, desc, suffix, label,
                                  desc_missing = FALSE) {
  # Variant to desc mapping
  desc_map <- c(
    brain = "brain",
    head = "head",
    mask = "brain",
    probseg = "probseg",
    dseg = "dseg"
  )

  # Modality/variant to suffix mapping
  suffix_map <- c(
    T1w = "T1w",
    T2w = "T2w",
    mask = "mask",
    probseg = "probseg",
    dseg = "dseg"
  )

  surface_suffixes <- c("inflated", "white", "pial", "midthickness", "sphere")

  # Track whether the user actually provided desc (non-NULL)
  user_supplied_desc <- !desc_missing && !is.null(desc)

  # Infer desc if not provided (or provided as NULL)
  final_desc <- desc
  if (!user_supplied_desc) {
    if (!is.null(variant) && variant %in% c("probseg", "dseg")) {
      final_desc <- NULL
    } else if (!is.null(variant) && variant %in% names(desc_map)) {
      final_desc <- unname(desc_map[variant])
    } else if (!is.null(suffix) &&
               suffix %in% c("probseg", "dseg", "mask", surface_suffixes)) {
      final_desc <- NULL
    } else if (is.null(variant)) {
      # Default volumetric desc when nothing else is specified
      final_desc <- desc_map[["brain"]]
    } else {
      # Unknown variant and no suffix guidance; force explicit desc
      final_desc <- NULL
    }
  }

  # Infer suffix if not provided
  final_suffix <- suffix
  if (is.null(final_suffix)) {
    # Prefer variant-driven suffix when it implies a specific file type
    if (!is.null(variant) && variant %in% names(suffix_map)) {
      final_suffix <- suffix_map[variant]
    } else if (!is.null(modality) && modality %in% names(suffix_map)) {
      final_suffix <- suffix_map[modality]
    }
  }

  # Validate we have required parameters
  if (is.null(final_desc)) {
    if (!is.null(final_suffix) && final_suffix %in% c("probseg", "dseg", "mask", surface_suffixes)) {
      # OK: these file types are fully determined by suffix
    } else {
      stop("Could not determine 'desc'. Provide explicitly or use a supported variant.")
    }
  }
  if (is.null(final_suffix)) {
    stop("Could not determine 'suffix'. Provide explicitly or use a supported variant/modality.")
  }

  # Warn about label usage
  if (!is.null(label) && !(final_suffix %in% c("probseg", "dseg"))) {
    warning("'label' typically used with suffix='probseg' or 'dseg', not '", final_suffix, "'")
  }

  return(list(desc = final_desc, suffix = final_suffix))
}

# Helper: Build query arguments for templateflow::tf_get()
.build_query_args <- function(space, resolution, desc, suffix,
                             label, atlas, cohort, extension, ...) {
  args <- list(
    template = space,
    desc = desc,
    suffix = suffix
  )

  # Add optional parameters
  if (!is.null(resolution)) args$resolution <- as.integer(resolution)
  if (!is.null(label)) args$label <- label
  if (!is.null(atlas)) args$atlas <- atlas
  if (!is.null(cohort)) args$cohort <- cohort
  if (!is.null(extension)) args$extension <- extension

  # Add extra arguments from ...
  extra_args <- list(...)
  for (name in names(extra_args)) {
    args[[name]] <- extra_args[[name]]
  }

  # Remove NULLs and sort for consistency
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    args <- args[sort(names(args))]
  }

  return(args)
}
