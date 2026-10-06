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
#' `reversible`, `data_files`, `status`, and `notes`. Artifact-backed routes
#' also
#' record `artifact_id`, `artifact_version`, `provider`, `url`, `sha256`,
#' `size_bytes`, `format`, `convention`, `qualification`, `qualification_scope`,
#' `qa_url`, and `license`. These fields are missing for routes without a
#' downloadable artifact.
#' `source_representation` and `target_representation` distinguish volumes and
#' surfaces. `executable` indicates support by the template-transform API, given
#' the required optional dependencies and artifacts; it is not a local readiness
#' check. Exact admitted surface routes also bind `from_domain_id`,
#' `to_domain_id`, `method`, `engine_revision`, `input_lock_sha256` and
#' hemisphere.
#' Broad surface names remain advisory. Numerical qualification is restricted to
#' the recorded inputs and methods; it does not establish anatomical accuracy.
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
#' Computes a transform route between spaces using the packaged
#' transform registry.
#'
#' Space identifiers are normalized internally, so aliases such as `"fslr32k"`
#' are accepted.
#'
#' @param from_space Source space identifier or a [surface_domain()] descriptor.
#' @param to_space Target space identifier or a [surface_domain()] descriptor.
#'   Supply descriptors for both endpoints for native surface resampling, or an
#'   exact volume-template identifier and target descriptor for projection.
#'   Only identical domain descriptors establish surface identity. Named surface
#'   routes remain advisory: template names do not bind exact meshes or methods.
#' @param data_type Data type being transformed (`"parcel"`, `"vertex"`,
#'   `"voxel"`). Voxel routes stay in volumes; vertex routes stay on surfaces.
#'   Parcel planning can also describe a single directed projection. Projection
#'   steps are not automatically composed with other routes.
#' @param mode Planning mode. `"auto"` returns `NULL` if no route exists,
#'   `"strict"` errors.
#' @param available_only Restrict routing to available edges. The default also
#'   shows planned routes for diagnostic compatibility. Execution always uses
#'   available, executable edges only; retired edges are never selected.
#' @param provider Provider filter, `"auto"`, `"neuroatlas"`, or
#'   `"templateflow"`.
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
atlas_transform_plan <- function(
  from_space,
  to_space,
  data_type = c("parcel", "vertex", "voxel"),
  mode = c("auto", "strict"),
  available_only = FALSE,
  provider = c("auto", "neuroatlas", "templateflow")
) {
  data_type <- match.arg(data_type)
  mode <- match.arg(mode)
  provider <- match.arg(provider)
  assertthat::assert_that(
    is.logical(available_only),
    length(available_only) == 1L,
    !is.na(available_only)
  )

  from_domain <- to_domain <- NULL
  from_surface <- inherits(from_space, "SurfaceDomain")
  to_surface <- inherits(to_space, "SurfaceDomain")
  typed <- from_surface || to_surface
  if (from_surface) {
    .validate_surface_domain(from_space)
    from_domain <- from_space
    from_space <- .surface_domain_space(from_domain)
  }
  if (to_surface) {
    .validate_surface_domain(to_space)
    to_domain <- to_space
    to_space <- .surface_domain_space(to_domain)
  }
  if (typed) {
    if (data_type == "voxel") stop("Surface domains cannot define voxel routes.")
    if (
      from_surface && to_surface &&
        from_domain$hemisphere != to_domain$hemisphere
    ) {
      stop("Surface routes must use the same hemisphere.")
    }
  }

  if (
    !is.character(from_space) || length(from_space) != 1L ||
      is.na(from_space) || !nzchar(from_space)
  ) {
    stop("'from_space' must be a non-empty character scalar")
  }
  if (
    !is.character(to_space) || length(to_space) != 1L ||
      is.na(to_space) || !nzchar(to_space)
  ) {
    stop("'to_space' must be a non-empty character scalar")
  }

  from_space <- .normalize_space_id(from_space)
  to_space <- .normalize_space_id(to_space)

  reg <- .space_route_capabilities(.space_transform_registry())
  surface_spaces <- unique(
    c(
      reg$from_space[reg$source_representation == "surface"],
      reg$to_space[reg$target_representation == "surface"]
    )
  )
  surface_identity <- typed || from_space %in% surface_spaces ||
    grepl(
      "^(fsaverage[0-9]*|fsLR([_-]?[0-9]+k)?)$",
      from_space,
      ignore.case = TRUE
    ) || data_type == "vertex"

  identity <- identical(from_space, to_space) &&
    (
      !surface_identity || (
        from_surface && to_surface &&
          identical(from_domain$id, to_domain$id)
      )
    )
  if (identical(from_space, to_space) && !identity) {
    message <- paste0(
      "Surface identity requires identical exact surface domains; ",
      "a template name or vertex count is insufficient."
    )
    if (mode == "strict") stop(message)
    warning(message, call. = FALSE)
    return(NULL)
  }
  if (identity) {
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
    return(
      structure(
        list(
          from_space = from_space,
          to_space = to_space,
          steps = step,
          n_steps = 1L,
          status = "available",
          confidence = "exact",
          from_domain = from_domain,
          to_domain = to_domain,
          warnings = character(0)
        ),
        class = c("atlas_transform_plan", "list")
      )
    )
  }

  reg <- reg[reg$status %in% if (available_only) {
    "available"
  } else {
    c("available", "candidate", "planned")
  }, , drop = FALSE]
  if (available_only) reg <- reg[reg$executable, , drop = FALSE]
  # Exact-domain entries never qualify a broad named alias or another ordering.
  exact <- !is.na(reg$from_domain_id) | !is.na(reg$to_domain_id)
  matches <- exact & reg$from_space == from_space & reg$to_space == to_space
  matches <- matches & if (from_surface) {
    !is.na(reg$from_domain_id) & reg$from_domain_id == from_domain$id
  } else {
    is.na(reg$from_domain_id)
  }
  matches <- matches & if (to_surface) {
    !is.na(reg$to_domain_id) & reg$to_domain_id == to_domain$id
  } else {
    is.na(reg$to_domain_id)
  }
  reg <- reg[!exact | matches, , drop = FALSE]
  if (
    data_type %in% c("voxel", "vertex") ||
      (from_surface && to_surface)
  ) {
    representation <- if (data_type == "voxel") "volume" else "surface"
    reg <- reg[
      reg$source_representation == representation &
        reg$target_representation == representation, , drop = FALSE
    ]
  }
  if (provider != "auto") {
    reg <- reg[
      reg$backend %in% c("identity", "internal_affine") |
        (!is.na(reg$provider) & reg$provider == provider), ,
      drop = FALSE
    ]
  }
  route <- .find_space_route(reg, from_space, to_space)
  if (!is.null(route)) {
    plan <- .build_transform_plan(from_space, to_space, route, data_type)
    plan$from_domain <- from_domain
    plan$to_domain <- to_domain
    if (
      typed && !all(
        !is.na(route$qualification) &
          route$qualification == "passed"
      )
    ) {
      plan$warnings <- c(
        plan$warnings,
        "Named route is advisory; exact domains and method are not qualified."
      )
    }
    return(structure(plan, class = c("atlas_transform_plan", "list")))
  }

  if (identical(mode, "strict")) {
    stop("No transform route found from '", from_space, "' to '", to_space, "'.")
  }

  warning(
    "No transform route found from '",
    from_space,
    "' to '",
    to_space,
    "'.",
    call. = FALSE
  )
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
  fields <- c(
    "artifact_id",
    "artifact_version",
    "provider",
    "url",
    "sha256",
    "format",
    "qualification",
    "qualification_scope",
    "qa_url",
    "license",
    "convention",
    "from_domain_id",
    "to_domain_id",
    "method",
    "engine_revision",
    "input_lock_sha256",
    "hemisphere"
  )
  for (field in setdiff(fields, names(reg))) reg[[field]] <- NA_character_
  if (!"size_bytes" %in% names(reg)) reg$size_bytes <- NA_real_
  .space_route_capabilities(reg)
}

# Capabilities describe registered API support, not software installation.
.space_route_capabilities <- function(reg) {
  for (field in c("from_domain_id", "to_domain_id", "qualification", "method")) {
    if (!field %in% names(reg)) reg[[field]] <- NA_character_
  }
  reg$source_representation <- ifelse(
    reg$transform_type %in% c("sphere_resample", "surf2vol"),
    "surface",
    "volume"
  )
  reg$target_representation <- ifelse(
    reg$transform_type %in% c("sphere_resample", "vol2surf"),
    "surface",
    "volume"
  )
  qualified_h5 <- rep(FALSE, nrow(reg))
  if (all(c("format", "qualification") %in% names(reg))) {
    qualified_h5 <- !is.na(reg$format) & reg$format == "ants_h5" &
      !is.na(reg$qualification) & reg$qualification == "passed"
  }
  volume <- reg$status == "available" &
    reg$source_representation == "volume" &
    reg$target_representation == "volume" &
    (reg$backend %in% c("identity", "internal_affine") | qualified_h5)
  native <- reg$backend == "neurotransform_native" &
    !is.na(reg$from_domain_id) & !is.na(reg$to_domain_id) &
    !is.na(reg$qualification) & reg$qualification == "passed" &
    !is.na(reg$method) & reg$method == "native_closest_barycentric"
  projection <- reg$backend == "cbig_registration_fusion" &
    !is.na(reg$to_domain_id) & !is.na(reg$qualification) &
    reg$qualification == "passed"
  reg$executable <- volume | (reg$status == "available" & (native | projection))
  reg
}

# Enumerate simple paths in the small, package-owned template graph. Rank all
# available paths before diagnostic candidates, then direct/fewer-hop routes,
# confidence, and stable artifact identity. Input CSV order never breaks ties.
.find_space_route <- function(reg, from_space, to_space) {
  projection <- reg$transform_type %in% c("vol2surf", "surf2vol")
  paths <- list()
  visit <- function(node, visited, indices) {
    outgoing <- which(reg$from_space == node & !reg$to_space %in% visited)
    for (i in outgoing) {
      # A projection needs its own frame, coverage, and conflict contract.
      # Do not discover implicit lossy shortcuts through another representation.
      if (length(indices) && (projection[[i]] || any(projection[indices]))) next
      next_indices <- c(indices, i)
      next_node <- reg$to_space[[i]]
      if (identical(next_node, to_space)) {
        paths[[length(paths) + 1L]] <<- reg[next_indices, , drop = FALSE]
      } else {
        visit(next_node, c(visited, next_node), next_indices)
      }
    }
  }
  visit(from_space, from_space, integer())
  if (!length(paths)) {
    return(NULL)
  }
  rank_status <- c(available = 0, candidate = 1, planned = 2)
  rank_conf <- c(exact = 0, high = 1, approximate = 2, uncertain = 3)
  status <- vapply(paths, function(p) max(rank_status[p$status]), numeric(1))
  confidence <- vapply(
    paths,
    function(p) {
      vals <- rank_conf[p$confidence]
      vals[is.na(vals)] <- 3
      max(vals)
    },
    numeric(1)
  )
  keys <- vapply(
    paths,
    function(p) {
      ids <- p$artifact_id
      if (is.null(ids)) ids <- rep(NA_character_, nrow(p))
      fallback <- paste(
        p$from_space,
        p$to_space,
        p$backend,
        p$data_files,
        sep = ":"
      )
      ids[is.na(ids)] <- fallback[is.na(ids)]
      paste(ids, collapse = "/")
    },
    character(1)
  )
  paths[[order(status, vapply(paths, nrow, integer(1)), confidence, keys)[[
    1L
  ]]]]
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
    reg$from_space == from_space & reg$to_space == to_space, ,
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

  combos <- lapply(
    mids,
    function(mid) {
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
    }
  )
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
  scores <- vapply(
    seq_len(nrow(routes)),
    function(i) {
      .route_score(routes[i, , drop = FALSE])
    },
    numeric(1)
  )
  routes[which.min(scores), , drop = FALSE]
}


#' @keywords internal
#' @noRd
.route_score <- function(route_df) {
  status_rank <- c(available = 0, candidate = 1, planned = 2, retired = 3)
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
  status_rank <- c(available = 0, candidate = 1, planned = 2, retired = 3)
  conf_rank <- c(exact = 0, high = 1, approximate = 2, uncertain = 3)
  status_names <- names(status_rank)
  conf_names <- names(conf_rank)

  warnings <- character(0)
  if (any(steps$status != "available")) {
    warnings <- c(warnings, "Plan includes unimplemented/planned transform step(s).")
  }
  if (any(steps$confidence %in% c("approximate", "uncertain"))) {
    warnings <- c(warnings, "Plan includes low-confidence transform step(s).")
  }
  if (
    identical(data_type, "vertex") &&
      any(steps$backend %in% c("sphere_nn", "nearest"))
  ) {
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
    download_bytes = if ("size_bytes" %in% names(steps)) {
      sum(steps$size_bytes)
    } else {
      NA_real_
    },
    status = total_status,
    confidence = total_conf,
    warnings = unique(warnings)
  )
}
