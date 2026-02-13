#' Space Transform Manifest
#'
#' Returns known transforms between coordinate/template spaces from a static
#' registry shipped with the package.
#'
#' @param status Optional character vector to filter by status
#'   (e.g., `"available"`, `"planned"`).
#'
#' @return
#' A data frame with one row per transform route and columns:
#' `from_space`, `to_space`, `transform_type`, `backend`, `confidence`,
#' `reversible`, `data_files`, `status`, and `notes`.
#'
#' @examples
#' # All known routes
#' reg <- space_transform_manifest()
#'
#' # Only implemented routes
#' available <- space_transform_manifest(status = "available")
#' @export
space_transform_manifest <- function(status = NULL) {
  reg <- .space_transform_registry()
  if (!is.null(status)) {
    reg <- reg[reg$status %in% status, , drop = FALSE]
  }
  rownames(reg) <- NULL
  reg
}


#' Plan a Transform Between Spaces
#'
#' Computes a direct or two-hop transform plan between spaces using the packaged
#' transform registry.
#'
#' Space identifiers are normalized internally, so aliases such as `"fslr32k"`
#' are accepted.
#'
#' @param from_space Source space identifier.
#' @param to_space Target space identifier.
#' @param data_type Data type being transformed (`"parcel"`, `"vertex"`,
#'   `"voxel"`). Used for advisory warnings.
#' @param mode Planning mode. `"auto"` returns `NULL` if no route exists,
#'   `"strict"` errors.
#'
#' @return
#' A list of class `"atlas_transform_plan"` with fields:
#' `from_space`, `to_space`, `steps`, `n_steps`, `status`, `confidence`,
#' and `warnings`.
#'
#' `steps` is a data frame with one row per transform step and registry columns.
#' In `mode = "auto"`, returns `NULL` (with warning) if no route exists.
#'
#' @examples
#' # Direct route
#' p1 <- atlas_transform_plan("MNI305", "MNI152")
#'
#' # Alias normalization + planned route
#' p2 <- atlas_transform_plan("fsaverage", "fslr32k")
#' p2$status
#' @export
atlas_transform_plan <- function(from_space,
                                 to_space,
                                 data_type = c("parcel", "vertex", "voxel"),
                                 mode = c("auto", "strict")) {
  data_type <- match.arg(data_type)
  mode <- match.arg(mode)

  if (!is.character(from_space) || length(from_space) != 1L || !nzchar(from_space)) {
    stop("'from_space' must be a non-empty character scalar")
  }
  if (!is.character(to_space) || length(to_space) != 1L || !nzchar(to_space)) {
    stop("'to_space' must be a non-empty character scalar")
  }

  from_space <- .normalize_space_id(from_space)
  to_space <- .normalize_space_id(to_space)

  if (identical(from_space, to_space)) {
    step <- data.frame(
      from_space = from_space,
      to_space = to_space,
      transform_type = "identity",
      backend = "identity",
      confidence = "exact",
      reversible = TRUE,
      data_files = NA_character_,
      status = "available",
      notes = "No transform required.",
      stringsAsFactors = FALSE
    )
    return(structure(
      list(
        from_space = from_space,
        to_space = to_space,
        steps = step,
        n_steps = 1L,
        status = "available",
        confidence = "exact",
        warnings = character(0)
      ),
      class = c("atlas_transform_plan", "list")
    ))
  }

  reg <- .space_transform_registry()
  direct <- .find_direct_space_route(reg, from_space, to_space)

  if (!is.null(direct)) {
    plan <- .build_transform_plan(from_space, to_space, direct, data_type)
    return(structure(plan, class = c("atlas_transform_plan", "list")))
  }

  two_hop <- .find_two_hop_space_route(reg, from_space, to_space)
  if (!is.null(two_hop)) {
    plan <- .build_transform_plan(from_space, to_space, two_hop, data_type)
    return(structure(plan, class = c("atlas_transform_plan", "list")))
  }

  if (identical(mode, "strict")) {
    stop("No transform route found from '", from_space, "' to '", to_space, "'.")
  }

  warning("No transform route found from '", from_space, "' to '", to_space, "'.",
          call. = FALSE)
  NULL
}


#' Print Method for Transform Plans
#'
#' @param x An `atlas_transform_plan` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' p <- atlas_transform_plan("MNI305", "MNI152")
#' print(p)
#' @export
print.atlas_transform_plan <- function(x, ...) {
  cat("<atlas_transform_plan>\n")
  cat("  from_space:", x$from_space, "\n")
  cat("  to_space:", x$to_space, "\n")
  cat("  n_steps:", x$n_steps, "\n")
  cat("  status:", x$status, "\n")
  cat("  confidence:", x$confidence, "\n")
  if (length(x$warnings) > 0L) {
    cat("  warnings:", paste(x$warnings, collapse = "; "), "\n")
  }
  invisible(x)
}


#' @keywords internal
#' @noRd
.space_transform_registry <- function() {
  path <- .transform_registry_path()
  reg <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("NA", ""))

  reg$from_space <- vapply(reg$from_space, .normalize_space_id, character(1))
  reg$to_space <- vapply(reg$to_space, .normalize_space_id, character(1))
  reg$confidence <- tolower(reg$confidence)
  reg$status <- tolower(reg$status)
  reg
}


#' @keywords internal
#' @noRd
.transform_registry_path <- function() {
  candidates <- c(
    system.file("extdata", "transform_registry.csv", package = "neuroatlas"),
    file.path(
      tryCatch(getNamespaceInfo("neuroatlas", "path"), error = function(e) ""),
      "extdata",
      "transform_registry.csv"
    ),
    file.path(
      tryCatch(getNamespaceInfo("neuroatlas", "path"), error = function(e) ""),
      "inst",
      "extdata",
      "transform_registry.csv"
    ),
    file.path("inst", "extdata", "transform_registry.csv"),
    file.path("..", "inst", "extdata", "transform_registry.csv")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("Could not locate transform registry CSV.")
  }
  hit[[1]]
}


#' @keywords internal
#' @noRd
.normalize_space_id <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }
  key <- tolower(gsub("[- ]", "", x))
  map <- c(
    "fslr" = "fsLR_32k",
    "fslr32k" = "fsLR_32k",
    "fslr_32k" = "fsLR_32k",
    "mni152nlin6asym" = "MNI152NLin6Asym",
    "mni152nlin2009casym" = "MNI152NLin2009cAsym",
    "mni152" = "MNI152",
    "mni305" = "MNI305",
    "fsaverage" = "fsaverage",
    "fsaverage5" = "fsaverage5",
    "fsaverage6" = "fsaverage6"
  )
  mapped <- unname(map[key])
  if (!is.na(mapped)) {
    return(mapped)
  }
  x
}


#' @keywords internal
#' @noRd
.find_direct_space_route <- function(reg, from_space, to_space) {
  hit <- reg[
    reg$from_space == from_space & reg$to_space == to_space,
    ,
    drop = FALSE
  ]
  if (nrow(hit) == 0L) {
    return(NULL)
  }
  .select_best_route(hit)
}


#' @keywords internal
#' @noRd
.find_two_hop_space_route <- function(reg, from_space, to_space) {
  from_edges <- reg[reg$from_space == from_space, , drop = FALSE]
  to_edges <- reg[reg$to_space == to_space, , drop = FALSE]

  if (nrow(from_edges) == 0L || nrow(to_edges) == 0L) {
    return(NULL)
  }

  mids <- intersect(unique(from_edges$to_space), unique(to_edges$from_space))
  if (length(mids) == 0L) {
    return(NULL)
  }

  combos <- lapply(mids, function(mid) {
    left <- .select_best_route(
      from_edges[from_edges$to_space == mid, , drop = FALSE]
    )
    right <- .select_best_route(
      to_edges[to_edges$from_space == mid, , drop = FALSE]
    )
    if (is.null(left) || is.null(right)) {
      return(NULL)
    }
    rbind(left, right)
  })
  combos <- Filter(Negate(is.null), combos)
  if (length(combos) == 0L) {
    return(NULL)
  }

  scores <- vapply(combos, .route_score, numeric(1))
  combos[[which.min(scores)]]
}


#' @keywords internal
#' @noRd
.select_best_route <- function(routes) {
  if (nrow(routes) == 0L) {
    return(NULL)
  }
  scores <- vapply(seq_len(nrow(routes)), function(i) {
    .route_score(routes[i, , drop = FALSE])
  }, numeric(1))
  routes[which.min(scores), , drop = FALSE]
}


#' @keywords internal
#' @noRd
.route_score <- function(route_df) {
  status_rank <- c(available = 0, planned = 1)
  conf_rank <- c(exact = 0, high = 1, approximate = 2, uncertain = 3)

  status_vals <- status_rank[route_df$status]
  status_vals[is.na(status_vals)] <- max(status_rank) + 1
  status_val <- max(status_vals)

  conf_vals <- conf_rank[route_df$confidence]
  conf_vals[is.na(conf_vals)] <- max(conf_rank) + 1
  conf_val <- max(conf_vals)

  step_penalty <- nrow(route_df) - 1
  status_val * 100 + conf_val * 10 + step_penalty
}


#' @keywords internal
#' @noRd
.build_transform_plan <- function(from_space, to_space, steps, data_type) {
  status_rank <- c(available = 0, planned = 1)
  conf_rank <- c(exact = 0, high = 1, approximate = 2, uncertain = 3)
  status_names <- names(status_rank)
  conf_names <- names(conf_rank)

  warnings <- character(0)
  if (any(steps$status == "planned")) {
    warnings <- c(warnings, "Plan includes unimplemented/planned transform step(s).")
  }
  if (any(steps$confidence %in% c("approximate", "uncertain"))) {
    warnings <- c(warnings, "Plan includes low-confidence transform step(s).")
  }
  if (identical(data_type, "vertex") &&
      any(steps$backend %in% c("sphere_nn", "nearest"))) {
    warnings <- c(
      warnings,
      "Nearest-neighbor surface resampling may be suboptimal for continuous data."
    )
  }

  status_vals <- status_rank[steps$status]
  status_vals[is.na(status_vals)] <- max(status_rank)
  conf_vals <- conf_rank[steps$confidence]
  conf_vals[is.na(conf_vals)] <- max(conf_rank)

  total_status <- status_names[max(status_vals) + 1L]
  total_conf <- conf_names[max(conf_vals) + 1L]

  list(
    from_space = from_space,
    to_space = to_space,
    steps = steps,
    n_steps = nrow(steps),
    status = total_status,
    confidence = total_conf,
    warnings = unique(warnings)
  )
}
