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
    parcels$id <- .normalise_parcel_id(parcels$id, "id", "parcel data")
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
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop("'strict' must be TRUE or FALSE")
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

  ids <- .normalise_parcel_id(parcels$id, "id", "parcel data")
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
    valid_count <- is.numeric(atlas$n_parcels) &&
      length(atlas$n_parcels) == 1L &&
      is.finite(atlas$n_parcels) &&
      atlas$n_parcels >= 0 &&
      atlas$n_parcels <= .Machine$integer.max &&
      atlas$n_parcels == trunc(atlas$n_parcels)
    if (!valid_count) {
      stop("'x$atlas$n_parcels' must be a non-negative integer when provided")
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
#'   - data frame/tibble with a stable parcel key and one or more value columns.
#'     Use `by` for renamed or composite keys.
#' @param value_col Column name used when `values` is a vector.
#' @param by Parcel-key specification used when `values` is a data frame.
#'   Unnamed values join columns with the same name, for example `"id"` or
#'   `c("label", "hemi", "network")`. A named character vector maps atlas
#'   metadata columns to columns in `values`, following dplyr join syntax, for
#'   example `c(id = "roi_index")`. When `NULL`, a safe unique key is inferred.
#' @param allow_partial Logical. If `FALSE` (default), a data-frame input must
#'   contain exactly one row for every atlas parcel. If `TRUE`, missing parcels
#'   are retained with `NA` values. Rows whose keys do not occur in the atlas
#'   always produce an error.
#' @param atlas_id Optional canonical atlas id override.
#' @param atlas_version Optional atlas version.
#' @param atlas_space Optional atlas space/template identifier.
#' @param schema_version Schema version for the returned object.
#' @export
as_parcel_data.atlas <- function(x,
                                 values = NULL,
                                 value_col = "value",
                                 by = NULL,
                                 allow_partial = FALSE,
                                 atlas_id = NULL,
                                 atlas_version = NULL,
                                 atlas_space = NULL,
                                 schema_version = "1.0.0",
                                 ...) {
  ref <- atlas_ref(x)

  meta <- roi_metadata(x)

  needed <- c("id", "label", "hemi")
  missing_needed <- setdiff(needed, names(meta))
  if (length(missing_needed) > 0L) {
    stop("roi_metadata(x) must include columns: ",
         paste(needed, collapse = ", "))
  }

  parcels <- tibble::as_tibble(meta)
  parcels$id <- .normalise_parcel_id(parcels$id, "id", "atlas metadata")

  if (!is.null(values)) {
    if ((is.numeric(values) || is.integer(values)) && is.null(dim(values))) {
      if (!is.null(by)) {
        stop("'by' is only used when 'values' is a data frame")
      }
      if (length(values) != nrow(parcels)) {
        stop("When 'values' is a vector, length(values) must match number of ",
             "parcels")
      }
      if (!is.character(value_col) || length(value_col) != 1L ||
          !nzchar(value_col)) {
        stop("'value_col' must be a non-empty character scalar")
      }
      if (value_col %in% names(parcels)) {
        stop("'value_col' cannot overwrite atlas metadata column '",
             value_col, "'")
      }
      parcels[[value_col]] <- values
    } else if (is.data.frame(values) || inherits(values, "parcel_data")) {
      aligned <- .align_atlas_table(
        atlas = x,
        data = values,
        by = by,
        allow_partial = allow_partial
      )
      parcels <- aligned$parcels
    } else {
      stop("'values' must be NULL, a numeric/integer vector, a data frame, ",
           "or a parcel_data object")
    }
  }

  if (is.null(atlas_id)) {
    atlas_id <- if (!is.null(x$name) && nzchar(x$name)) {
      x$name
    } else {
      class(x)[1]
    }
  }

  if (is.null(atlas_space) && !is.null(ref$template_space) &&
      !is.na(ref$template_space) && nzchar(ref$template_space)) {
    atlas_space <- ref$template_space
  }

  atlas_ref <- list(
    id = atlas_id,
    name = if (!is.null(x$name)) x$name else atlas_id,
    version = atlas_version,
    space = atlas_space,
    class = class(x)[1],
    family = ref$family,
    model = ref$model,
    representation = ref$representation,
    coord_space = ref$coord_space,
    confidence = ref$confidence,
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

#' Align a Parcel Value Column to an Atlas
#'
#' Match a data-frame column to atlas parcels and return a numeric vector in
#' exact `atlas$ids` order. Matching is strict by default: atlas and data keys
#' must be unique, data keys must all exist in the atlas, and every atlas parcel
#' must be represented unless `allow_partial = TRUE`.
#'
#' @param atlas An atlas object.
#' @param data A data frame, tibble, or `parcel_data` object containing parcel
#'   keys and values.
#' @param value A numeric value column, supplied as a bare name or string.
#' @param by Parcel-key specification. Use a shared column name such as `"id"`,
#'   or a named character vector to map an atlas metadata column to a differently
#'   named data column, for example `c(id = "roi_index")`. Composite keys are
#'   supported. When `NULL`, a safe unique key is inferred.
#' @param allow_partial Logical. If `FALSE` (default), `data` must contain every
#'   atlas parcel. If `TRUE`, unmatched atlas parcels receive `NA`. Unknown and
#'   duplicate data keys always error.
#'
#' @return A numeric vector named by parcel ID and ordered exactly like
#'   `atlas$ids`.
#'
#' @details
#' With `by = NULL`, `id` is preferred, followed by `label_full`. Composite
#' label keys are considered only when they are unique in both the atlas and
#' data. Ambiguous short labels therefore fail instead of being recycled across
#' hemispheres or networks.
#'
#' Atlas metadata columns supplied in `data` are treated as consistency checks.
#' They are never allowed to overwrite canonical metadata.
#'
#' @examples
#' atlas <- structure(
#'   list(
#'     name = "toy",
#'     atlas = array(1:3, c(3, 1, 1)),
#'     ids = 1:3,
#'     labels = c("A", "B", "C"),
#'     orig_labels = c("lh_A", "lh_B", "rh_C"),
#'     hemi = c("left", "left", "right")
#'   ),
#'   class = "atlas"
#' )
#' results <- data.frame(
#'   roi_index = c(3L, 1L, 2L),
#'   estimate = c(0.3, 0.1, 0.2)
#' )
#' align_parcel_values(
#'   atlas,
#'   results,
#'   value = estimate,
#'   by = c(id = "roi_index")
#' )
#'
#' @export
align_parcel_values <- function(atlas,
                                data,
                                value,
                                by = NULL,
                                allow_partial = FALSE) {
  value_quo <- rlang::enquo(value)
  value_col <- .parcel_value_column(value_quo)

  aligned <- .align_atlas_table(
    atlas = atlas,
    data = data,
    by = by,
    allow_partial = allow_partial
  )

  .parcel_value_from_aligned(aligned, atlas, value_col)
}


#' Extract and validate one numeric value column from aligned parcels
#' @keywords internal
#' @noRd
.parcel_value_from_aligned <- function(aligned, atlas, value_col) {

  if (!value_col %in% aligned$value_cols) {
    if (value_col %in% names(aligned$parcels)) {
      cli::cli_abort(
        c(
          "{.arg value} must select a data value column, not atlas metadata.",
          "i" = "{.val {value_col}} is a canonical atlas column."
        ),
        class = c("neuroatlas_error_parcel_value", "neuroatlas_error")
      )
    }
    cli::cli_abort(
      "Column {.val {value_col}} was not found in {.arg data}.",
      class = c("neuroatlas_error_parcel_value", "neuroatlas_error")
    )
  }

  vals <- aligned$parcels[[value_col]]
  if ((!is.numeric(vals) && !is.integer(vals)) || !is.null(dim(vals))) {
    cli::cli_abort(
      "Parcel value column {.val {value_col}} must be a numeric vector.",
      class = c("neuroatlas_error_parcel_value", "neuroatlas_error")
    )
  }

  stats::setNames(vals, as.character(atlas$ids))
}


#' Resolve a value-column expression to one column name
#' @keywords internal
#' @noRd
.parcel_value_column <- function(value_quo) {
  if (rlang::quo_is_missing(value_quo) || rlang::quo_is_null(value_quo)) {
    cli::cli_abort(
      "{.arg value} must name one numeric column in {.arg data}.",
      class = c("neuroatlas_error_parcel_value", "neuroatlas_error")
    )
  }

  expr <- rlang::get_expr(value_quo)
  if (rlang::is_symbol(expr)) {
    return(rlang::as_string(expr))
  }
  if (is.character(expr) && length(expr) == 1L && nzchar(expr)) {
    return(expr)
  }

  cli::cli_abort(
    "{.arg value} must be a bare column name or one character string.",
    class = c("neuroatlas_error_parcel_value", "neuroatlas_error")
  )
}


#' Align a data frame to canonical atlas row order
#' @keywords internal
#' @noRd
.align_atlas_table <- function(atlas,
                               data,
                               by = NULL,
                               allow_partial = FALSE) {
  if (!inherits(atlas, "atlas")) {
    cli::cli_abort(
      "{.arg atlas} must inherit from class {.cls atlas}.",
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }
  if (!is.logical(allow_partial) || length(allow_partial) != 1L ||
      is.na(allow_partial)) {
    cli::cli_abort(
      "{.arg allow_partial} must be `TRUE` or `FALSE`.",
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }

  if (inherits(data, "parcel_data")) {
    validate_parcel_data(data)
    .validate_parcel_atlas_identity(data, atlas)
    data <- data$parcels
  }
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data frame, tibble, or {.cls parcel_data} object.",
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }

  data <- tibble::as_tibble(data)
  meta <- tibble::as_tibble(roi_metadata(atlas))
  if (nrow(data) == 0L) {
    cli::cli_abort(
      "{.arg data} must contain at least one parcel row.",
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }
  if (nrow(meta) == 0L) {
    cli::cli_abort(
      "{.arg atlas} does not contain any parcel metadata rows.",
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }
  if (!"id" %in% names(meta)) {
    cli::cli_abort(
      "{.arg atlas} metadata must contain an {.field id} column.",
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }
  atlas_ids <- .normalise_parcel_id(atlas$ids, "id", "atlas")
  meta_ids <- .normalise_parcel_id(meta$id, "id", "atlas metadata")
  if (anyDuplicated(atlas_ids) || anyDuplicated(meta_ids) ||
      !identical(atlas_ids, meta_ids)) {
    cli::cli_abort(
      c(
        "{.arg atlas} has inconsistent parcel IDs.",
        "i" = paste0(
          "{.code roi_metadata(atlas)$id} must be unique and match ",
          "{.code atlas$ids} in order."
        )
      ),
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }
  meta$id <- meta_ids
  key <- .resolve_parcel_by(meta, data, by = by)
  atlas_key <- .normalise_parcel_key(meta, key$atlas, key$atlas, "atlas")
  data_key <- .normalise_parcel_key(data, key$data, key$atlas, "data")
  codes <- .parcel_key_codes(atlas_key, data_key)
  atlas_codes <- codes$atlas
  data_codes <- codes$data
  by_text <- paste(key$atlas, collapse = ", ")

  if (anyDuplicated(atlas_codes) > 0L) {
    cli::cli_abort(
      c(
        "Atlas key ({by_text}) is not unique.",
        "i" = "Use {.field id}, {.field label_full}, or a more specific composite key."
      ),
      class = c("neuroatlas_error_ambiguous_parcel_key", "neuroatlas_error")
    )
  }
  if (anyDuplicated(data_codes) > 0L) {
    cli::cli_abort(
      "Data key ({by_text}) must identify each row uniquely.",
      class = c("neuroatlas_error_duplicate_parcel_key", "neuroatlas_error")
    )
  }

  data_to_atlas <- match(data_codes, atlas_codes)
  if (anyNA(data_to_atlas)) {
    bad <- which(is.na(data_to_atlas))
    cli::cli_abort(
      c(
        "{length(bad)} data parcel row{?s} did not match the atlas.",
        "i" = "All supplied parcel keys must belong to this atlas.",
        "i" = "Join key: {by_text}."
      ),
      class = c("neuroatlas_error_unknown_parcel_key", "neuroatlas_error")
    )
  }

  atlas_to_data <- match(atlas_codes, data_codes)
  missing_atlas <- which(is.na(atlas_to_data))
  if (length(missing_atlas) > 0L && !allow_partial) {
    cli::cli_abort(
      c(
        "Data are missing {length(missing_atlas)} atlas parcel{?s}.",
        "i" = paste0(
          "Set {.code allow_partial = TRUE} to retain unmatched parcels ",
          "as {.code NA}."
        ),
        "i" = "Join key: {by_text}."
      ),
      class = c("neuroatlas_error_missing_parcel_key", "neuroatlas_error")
    )
  }

  verification_cols <- setdiff(
    intersect(names(data), names(meta)),
    key$data
  )
  for (col in verification_cols) {
    expected <- meta[[col]][data_to_atlas]
    supplied <- data[[col]]
    equal <- .parcel_metadata_equal(expected, supplied)
    if (any(!equal)) {
      cli::cli_abort(
        c(
          "Data column {.field {col}} disagrees with canonical atlas metadata.",
          "i" = paste0(
            "Atlas metadata are verified during alignment and are never ",
            "overwritten."
          )
        ),
        class = c("neuroatlas_error_parcel_metadata", "neuroatlas_error")
      )
    }
  }

  value_cols <- setdiff(
    names(data),
    union(key$data, verification_cols)
  )
  parcels <- meta
  for (col in value_cols) {
    parcels[[col]] <- .subset_parcel_column(data[[col]], atlas_to_data)
  }

  list(
    parcels = parcels,
    by = stats::setNames(key$data, key$atlas),
    value_cols = value_cols,
    missing_ids = meta$id[missing_atlas]
  )
}


#' Reorder a table column without flattening matrix-like columns
#' @keywords internal
#' @noRd
.subset_parcel_column <- function(x, index) {
  if (is.null(dim(x))) {
    return(x[index])
  }
  dimensions <- length(dim(x))
  args <- c(
    list(x, index),
    rep(list(TRUE), dimensions - 1L),
    list(drop = FALSE)
  )
  do.call(`[`, args)
}


#' Resolve an explicit or safely inferred parcel join key
#' @keywords internal
#' @noRd
.resolve_parcel_by <- function(meta, data, by = NULL) {
  if (!is.null(by)) {
    if (!is.character(by) || length(by) == 0L || anyNA(by) ||
        any(!nzchar(by))) {
      cli::cli_abort(
        "{.arg by} must be a non-empty character vector.",
        class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
      )
    }
    by_names <- names(by)
    if (is.null(by_names) || all(!nzchar(by_names))) {
      atlas_cols <- unname(by)
      data_cols <- unname(by)
    } else if (all(nzchar(by_names))) {
      atlas_cols <- by_names
      data_cols <- unname(by)
    } else {
      cli::cli_abort(
        "{.arg by} must be either entirely named or entirely unnamed.",
        class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
      )
    }
    if (anyDuplicated(atlas_cols) || anyDuplicated(data_cols)) {
      cli::cli_abort(
        "{.arg by} cannot contain duplicate atlas or data columns.",
        class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
      )
    }
    missing_atlas <- setdiff(atlas_cols, names(meta))
    missing_data <- setdiff(data_cols, names(data))
    if (length(missing_atlas) || length(missing_data)) {
      cli::cli_abort(
        c(
          "Parcel join columns were not found.",
          "x" = "Missing atlas columns: {paste(missing_atlas, collapse = ', ')}.",
          "x" = "Missing data columns: {paste(missing_data, collapse = ', ')}."
        ),
        class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
      )
    }
    return(list(atlas = atlas_cols, data = data_cols))
  }

  if ("id" %in% names(meta) && "id" %in% names(data)) {
    return(list(atlas = "id", data = "id"))
  }
  if ("label_full" %in% names(meta) && "label_full" %in% names(data)) {
    return(list(atlas = "label_full", data = "label_full"))
  }

  candidates <- list(
    c("label", "hemi", "network"),
    c("label", "hemi"),
    "label"
  )
  for (cols in candidates) {
    if (!all(cols %in% names(meta)) || !all(cols %in% names(data))) next
    atlas_key <- .normalise_parcel_key(meta, cols, cols, "atlas")
    data_key <- .normalise_parcel_key(data, cols, cols, "data")
    codes <- .parcel_key_codes(atlas_key, data_key)
    if (!anyDuplicated(codes$atlas) && !anyDuplicated(codes$data)) {
      return(list(atlas = cols, data = cols))
    }
  }

  cli::cli_abort(
    c(
      "Could not infer a safe unique parcel key.",
      "i" = paste0(
        "Prefer an integer atlas ID and use ",
        "{.code by = c(id = 'roi_index')} for renamed columns."
      ),
      "i" = "Full labels or a unique label/hemi/network composite are also supported."
    ),
    class = c("neuroatlas_error_ambiguous_parcel_key", "neuroatlas_error")
  )
}


#' Normalise atlas and data key columns to comparable values
#' @keywords internal
#' @noRd
.normalise_parcel_key <- function(x, source_cols, atlas_cols, side) {
  out <- vector("list", length(source_cols))
  names(out) <- atlas_cols
  for (i in seq_along(source_cols)) {
    value <- x[[source_cols[[i]]]]
    if (identical(atlas_cols[[i]], "id")) {
      out[[i]] <- .normalise_parcel_id(value, source_cols[[i]], side)
    } else {
      if (is.list(value) || is.matrix(value) || is.data.frame(value)) {
        cli::cli_abort(
          "Parcel key column {.field {source_cols[[i]]}} in {side} must be atomic.",
          class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
        )
      }
      value <- as.character(value)
      if (any(!is.na(value) & !nzchar(value))) {
        cli::cli_abort(
          paste0(
            "Parcel key column {.field {source_cols[[i]]}} in {side} ",
            "cannot contain empty strings."
          ),
          class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
        )
      }
      out[[i]] <- value
    }
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


#' Normalise integer parcel identifiers without truncation
#' @keywords internal
#' @noRd
.normalise_parcel_id <- function(x, column, side) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- trimws(x)
    valid <- !is.na(x) & grepl("^[+-]?[0-9]+$", x)
    if (!all(valid)) {
      cli::cli_abort(
        "Parcel ID column {.field {column}} in {side} must contain integers.",
        class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
      )
    }
    out <- suppressWarnings(as.integer(x))
  } else if (is.numeric(x) || is.integer(x)) {
    valid <- !is.na(x) & is.finite(x) & x == trunc(x)
    if (!all(valid)) {
      cli::cli_abort(
        "Parcel ID column {.field {column}} in {side} must contain integers.",
        class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
      )
    }
    out <- suppressWarnings(as.integer(x))
  } else {
    cli::cli_abort(
      "Parcel ID column {.field {column}} in {side} must be integer-like.",
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }
  if (anyNA(out)) {
    cli::cli_abort(
      paste0(
        "Parcel ID column {.field {column}} in {side} is outside the ",
        "supported integer range."
      ),
      class = c("neuroatlas_error_parcel_key", "neuroatlas_error")
    )
  }
  out
}


#' Encode composite parcel keys without string concatenation collisions
#' @keywords internal
#' @noRd
.parcel_key_codes <- function(atlas_key, data_key) {
  combined <- rbind(atlas_key, data_key)
  factors <- lapply(combined, factor, exclude = NULL)
  codes <- do.call(
    interaction,
    c(factors, list(drop = TRUE, lex.order = TRUE))
  )
  codes <- as.integer(codes)
  n_atlas <- nrow(atlas_key)
  list(
    atlas = codes[seq_len(n_atlas)],
    data = codes[n_atlas + seq_len(nrow(data_key))]
  )
}


#' Compare supplied and canonical metadata, treating paired NA as equal
#' @keywords internal
#' @noRd
.parcel_metadata_equal <- function(expected, supplied) {
  expected <- as.character(expected)
  supplied <- as.character(supplied)
  equal <- expected == supplied | (is.na(expected) & is.na(supplied))
  equal[is.na(equal)] <- FALSE
  equal
}


#' Check parcel_data provenance fields that are available on both objects
#' @keywords internal
#' @noRd
.validate_parcel_atlas_identity <- function(data, atlas) {
  ref <- atlas_ref(atlas)
  comparisons <- list(
    family = ref$family,
    model = ref$model,
    representation = ref$representation,
    space = ref$template_space
  )
  for (field in names(comparisons)) {
    supplied <- data$atlas[[field]]
    expected <- comparisons[[field]]
    present <- !is.null(supplied) && length(supplied) == 1L &&
      !is.na(supplied) && nzchar(as.character(supplied)) &&
      !is.null(expected) && length(expected) == 1L &&
      !is.na(expected) && nzchar(as.character(expected))
    if (present && !identical(as.character(supplied), as.character(expected))) {
      cli::cli_abort(
        c(
          "{.cls parcel_data} atlas identity does not match {.arg atlas}.",
          "x" = "{field}: supplied {.val {supplied}}, expected {.val {expected}}."
        ),
        class = c("neuroatlas_error_atlas_identity", "neuroatlas_error")
      )
    }
  }
  invisible(TRUE)
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
  if (!is.character(column) || length(column) != 1L || !nzchar(column)) {
    stop("'column' must be a non-empty character scalar")
  }
  if (!column %in% names(x$parcels)) {
    stop("Column '", column, "' not found in x$parcels")
  }

  aligned <- .align_atlas_table(
    atlas = atlas,
    data = x,
    by = "id",
    allow_partial = TRUE
  )
  aligned$parcels[[column]]
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
