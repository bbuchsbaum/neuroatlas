#' Parcel-Level Data Container
#'
#' Create a validated, serializable parcel-level data object.
#'
#' @param parcels A data frame or tibble with one row per parcel.
#' @param atlas_id Canonical parcellation identifier.
#' @param atlas_name Human-readable parcellation name. Defaults to `atlas_id`.
#' @param atlas_version Optional atlas version string.
#' @param atlas_space Optional template/space identifier.
#' @param schema_version Schema version string. Default: `"1.0.0"`.
#'
#' @details
#' `parcel_data` formalizes reduced parcel representations as two components:
#' - atlas identity metadata (`atlas`)
#' - a parcel table (`parcels`) with required columns `id`, `label`, and `hemi`
#'
#' Additional columns in `parcels` are interpreted as value or feature columns.
#'
#' @return An object of class `"parcel_data"`.
#'
#' @examples
#' tbl <- tibble::tibble(
#'   id = c(1L, 2L),
#'   label = c("A", "B"),
#'   hemi = c("left", "right"),
#'   statistic = c(0.4, -0.2)
#' )
#' x <- parcel_data(tbl, atlas_id = "toy_atlas")
#' x
#'
#' @seealso [as_parcel_data()], [write_parcel_data()], [read_parcel_data()]
#' @export
parcel_data <- function(parcels,
                        atlas_id,
                        atlas_name = atlas_id,
                        atlas_version = NULL,
                        atlas_space = NULL,
                        schema_version = "1.0.0") {
  if (missing(atlas_id) || !is.character(atlas_id) || length(atlas_id) != 1L ||
      !nzchar(atlas_id)) {
    stop("'atlas_id' must be a non-empty character scalar")
  }

  parcels <- tibble::as_tibble(parcels)
  if ("id" %in% names(parcels)) {
    parcels$id <- as.integer(parcels$id)
  }

  x <- list(
    schema_version = schema_version,
    atlas = list(
      id = atlas_id,
      name = atlas_name,
      version = atlas_version,
      space = atlas_space,
      n_parcels = nrow(parcels)
    ),
    parcels = parcels
  )
  class(x) <- c("parcel_data", "list")

  validate_parcel_data(x)
  x
}

#' Validate a Parcel-Level Data Object
#'
#' Validate structure and key invariants for `parcel_data` objects.
#'
#' @param x An object expected to be `parcel_data`.
#' @param strict Logical. If `TRUE` (default), enforce strict checks on atlas
#'   metadata consistency.
#'
#' @return Invisibly returns `x` if valid; otherwise throws an error.
#' @export
validate_parcel_data <- function(x, strict = TRUE) {
  if (!inherits(x, "parcel_data")) {
    stop("'x' must inherit from class 'parcel_data'")
  }

  required_top <- c("schema_version", "atlas", "parcels")
  missing_top <- setdiff(required_top, names(x))
  if (length(missing_top) > 0L) {
    stop("Missing required fields in parcel_data: ",
         paste(missing_top, collapse = ", "))
  }

  atlas <- x$atlas
  if (!is.list(atlas)) {
    stop("'x$atlas' must be a list")
  }
  if (is.null(atlas$id) || !is.character(atlas$id) || length(atlas$id) != 1L ||
      !nzchar(atlas$id)) {
    stop("'x$atlas$id' must be a non-empty character scalar")
  }

  parcels <- x$parcels
  if (!is.data.frame(parcels)) {
    stop("'x$parcels' must be a data frame")
  }

  required_cols <- c("id", "label", "hemi")
  missing_cols <- setdiff(required_cols, names(parcels))
  if (length(missing_cols) > 0L) {
    stop("'x$parcels' is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!is.numeric(parcels$id) && !is.integer(parcels$id)) {
    stop("'x$parcels$id' must be numeric/integer")
  }

  ids <- as.integer(parcels$id)
  if (anyNA(ids)) {
    stop("'x$parcels$id' cannot contain NA values")
  }
  if (anyDuplicated(ids) > 0L) {
    stop("'x$parcels$id' must be unique")
  }

  if (!is.character(parcels$label)) {
    stop("'x$parcels$label' must be character")
  }
  if (!is.character(parcels$hemi) && !all(is.na(parcels$hemi))) {
    stop("'x$parcels$hemi' must be character or NA")
  }

  if (isTRUE(strict) && !is.null(atlas$n_parcels)) {
    if (!is.numeric(atlas$n_parcels) || length(atlas$n_parcels) != 1L) {
      stop("'x$atlas$n_parcels' must be a numeric scalar when provided")
    }
    if (as.integer(atlas$n_parcels) != nrow(parcels)) {
      stop("'x$atlas$n_parcels' does not match number of parcel rows")
    }
  }

  invisible(x)
}

#' Convert an Object to `parcel_data`
#'
#' @param x Object to convert.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `"parcel_data"`.
#' @export
as_parcel_data <- function(x, ...) {
  UseMethod("as_parcel_data")
}

#' @rdname as_parcel_data
#' @export
as_parcel_data.parcel_data <- function(x, ...) {
  validate_parcel_data(x)
  x
}

#' @rdname as_parcel_data
#' @param values Optional values to attach to parcel rows.
#'   - numeric/integer vector of length `length(x$ids)`
#'   - data frame/tibble with join column `id` or `label` and one or more value
#'     columns
#' @param value_col Column name used when `values` is a vector.
#' @param atlas_id Optional canonical atlas id override.
#' @param atlas_version Optional atlas version.
#' @param atlas_space Optional atlas space/template identifier.
#' @param schema_version Schema version for the returned object.
#' @export
as_parcel_data.atlas <- function(x,
                                 values = NULL,
                                 value_col = "value",
                                 atlas_id = NULL,
                                 atlas_version = NULL,
                                 atlas_space = NULL,
                                 schema_version = "1.0.0",
                                 ...) {
  meta <- roi_metadata(x)

  needed <- c("id", "label", "hemi")
  missing_needed <- setdiff(needed, names(meta))
  if (length(missing_needed) > 0L) {
    stop("roi_metadata(x) must include columns: ",
         paste(needed, collapse = ", "))
  }

  parcels <- tibble::as_tibble(meta)
  parcels$id <- as.integer(parcels$id)

  if (!is.null(values)) {
    if (is.numeric(values) || is.integer(values)) {
      if (length(values) != nrow(parcels)) {
        stop("When 'values' is a vector, length(values) must match number of ",
             "parcels")
      }
      parcels[[value_col]] <- values
    } else if (is.data.frame(values)) {
      values_tbl <- tibble::as_tibble(values)
      join_col <- if ("id" %in% names(values_tbl)) {
        "id"
      } else if ("label" %in% names(values_tbl)) {
        "label"
      } else {
        stop("When 'values' is a data frame it must include 'id' or 'label'")
      }

      if (anyDuplicated(values_tbl[[join_col]]) > 0L) {
        stop("Join column '", join_col, "' in 'values' must contain unique ",
             "keys")
      }

      if (join_col == "id") {
        values_tbl$id <- as.integer(values_tbl$id)
      }

      idx <- match(parcels[[join_col]], values_tbl[[join_col]])
      add_cols <- setdiff(names(values_tbl), join_col)
      for (col in add_cols) {
        parcels[[col]] <- values_tbl[[col]][idx]
      }
    } else {
      stop("'values' must be NULL, numeric/integer vector, or data frame")
    }
  }

  if (is.null(atlas_id)) {
    atlas_id <- if (!is.null(x$name) && nzchar(x$name)) {
      x$name
    } else {
      class(x)[1]
    }
  }

  atlas_ref <- list(
    id = atlas_id,
    name = if (!is.null(x$name)) x$name else atlas_id,
    version = atlas_version,
    space = atlas_space,
    class = class(x)[1],
    n_parcels = nrow(parcels)
  )

  out <- list(
    schema_version = schema_version,
    atlas = atlas_ref,
    parcels = parcels
  )
  class(out) <- c("parcel_data", "list")

  validate_parcel_data(out)
  out
}

#' @rdname as_parcel_data
#' @export
as_parcel_data.default <- function(x, ...) {
  stop("No as_parcel_data() method for objects of class: ",
       paste(class(x), collapse = ", "))
}

#' Extract Parcel Values Aligned to an Atlas
#'
#' Returns a vector aligned to `atlas$ids`, suitable for `map_atlas()` or
#' `plot_brain()`.
#'
#' @param x A `parcel_data` object.
#' @param atlas An atlas object.
#' @param column Value column in `x$parcels` to extract.
#'
#' @return A vector with `length(atlas$ids)` elements ordered to `atlas$ids`.
#' @export
parcel_values <- function(x, atlas, column = "value") {
  x <- as_parcel_data(x)
  if (!inherits(atlas, "atlas")) {
    stop("'atlas' must inherit from class 'atlas'")
  }
  if (!column %in% names(x$parcels)) {
    stop("Column '", column, "' not found in x$parcels")
  }

  vals <- x$parcels[[column]][rep(NA_integer_, length(atlas$ids))]
  idx <- match(as.integer(atlas$ids), as.integer(x$parcels$id))
  keep <- which(!is.na(idx))
  vals[keep] <- x$parcels[[column]][idx[keep]]
  vals
}

#' Write a `parcel_data` Object to Disk
#'
#' @param x A `parcel_data` object.
#' @param file Output file path.
#' @param format Serialization format: `"auto"`, `"rds"`, or `"json"`.
#' @param pretty Logical; pretty-print JSON output when `format = "json"`.
#'
#' @return Invisibly returns normalized output path.
#' @export
write_parcel_data <- function(x,
                              file,
                              format = c("auto", "rds", "json"),
                              pretty = TRUE) {
  x <- as_parcel_data(x)
  format <- match.arg(format)
  format <- .resolve_parcel_data_format(format, file)

  out_dir <- dirname(file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (format == "rds") {
    saveRDS(x, file = file)
  } else {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required for JSON serialization")
    }
    json_obj <- unclass(x)
    json_obj$parcels <- as.data.frame(json_obj$parcels,
                                      stringsAsFactors = FALSE)
    jsonlite::write_json(json_obj,
                         path = file,
                         auto_unbox = TRUE,
                         pretty = pretty,
                         null = "null",
                         na = "null")
  }

  invisible(normalizePath(file, mustWork = FALSE))
}

#' Read a `parcel_data` Object from Disk
#'
#' @param file Input file path.
#' @param format Serialization format: `"auto"`, `"rds"`, or `"json"`.
#' @param validate Logical. If `TRUE` (default), validate after reading.
#'
#' @return A `parcel_data` object.
#' @export
read_parcel_data <- function(file,
                             format = c("auto", "rds", "json"),
                             validate = TRUE) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  format <- match.arg(format)
  format <- .resolve_parcel_data_format(format, file)

  if (format == "rds") {
    obj <- readRDS(file)
  } else {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required for JSON deserialization")
    }
    obj <- jsonlite::read_json(path = file, simplifyVector = TRUE)
  }

  if (inherits(obj, "parcel_data")) {
    out <- obj
  } else if (is.list(obj)) {
    out <- list(
      schema_version = obj$schema_version,
      atlas = obj$atlas,
      parcels = tibble::as_tibble(obj$parcels)
    )
    class(out) <- c("parcel_data", "list")
  } else {
    stop("Serialized object is not a recognized parcel_data structure")
  }

  if (isTRUE(validate)) {
    validate_parcel_data(out)
  }
  out
}

#' @keywords internal
#' @noRd
.resolve_parcel_data_format <- function(format, file) {
  if (format != "auto") {
    return(format)
  }

  ext <- tolower(tools::file_ext(file))
  if (ext == "rds") {
    return("rds")
  }
  if (ext == "json") {
    return("json")
  }

  stop("Could not infer parcel_data format from file extension: '", ext,
       "'. Use format = 'rds' or format = 'json'.")
}

#' @export
print.parcel_data <- function(x, ...) {
  validate_parcel_data(x, strict = FALSE)

  cat("parcel_data", "\n", sep = "")
  cat("  schema:", x$schema_version, "\n")
  cat("  atlas:", x$atlas$id, "\n")
  cat("  parcels:", nrow(x$parcels), "\n")

  value_cols <- setdiff(names(x$parcels), c("id", "label", "hemi"))
  if (length(value_cols) > 0L) {
    cat("  value_cols:", paste(value_cols, collapse = ", "), "\n")
  }

  invisible(x)
}
