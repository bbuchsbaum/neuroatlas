# Atlas utilities for the cluster explorer
# Split from cluster_explorer.R

.has_surface_geometry <- function(surfatlas) {
  if (!inherits(surfatlas, "surfatlas")) {
    return(FALSE)
  }

  ok_hemi <- vapply(c("lh_atlas", "rh_atlas"), function(hk) {
    obj <- surfatlas[[hk]]
    if (is.null(obj) || !isS4(obj)) {
      return(FALSE)
    }
    all(c("geometry", "data") %in% methods::slotNames(obj))
  }, logical(1))

  all(ok_hemi)
}

.warn_if_atlas_surface_mismatch <- function(atlas, surfatlas) {
  if (is.null(atlas) || is.null(surfatlas)) {
    return(invisible(NULL))
  }

  atlas_ids <- suppressWarnings(as.integer(atlas$ids))
  surf_ids <- suppressWarnings(as.integer(surfatlas$ids))

  atlas_ids <- unique(atlas_ids[is.finite(atlas_ids)])
  surf_ids <- unique(surf_ids[is.finite(surf_ids)])

  if (length(atlas_ids) == 0 || length(surf_ids) == 0) {
    return(invisible(NULL))
  }

  overlap <- intersect(atlas_ids, surf_ids)
  n_overlap <- length(overlap)

  if (n_overlap == 0) {
    warning(
      "No overlapping ROI IDs between `atlas` and `surfatlas`. ",
      "Use matching atlas variants (e.g., Schaefer 400/17 with Schaefer ",
      "surface 400/17).",
      call. = FALSE
    )
    return(invisible(NULL))
  }

  if (!setequal(atlas_ids, surf_ids)) {
    warning(
      "`atlas` and `surfatlas` ROI IDs partially overlap (",
      n_overlap, "/", length(atlas_ids), " atlas IDs; ",
      n_overlap, "/", length(surf_ids), " surface IDs). ",
      "Results may be difficult to interpret unless both atlases match.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

.infer_surfatlas_from_atlas <- function(atlas) {
  if (is.null(atlas) || !is.list(atlas)) {
    return(NULL)
  }

  embedded <- tryCatch(atlas$surfatlas, error = function(e) NULL)
  if (is.list(embedded) && inherits(embedded, "surfatlas")) {
    return(embedded)
  }

  if (inherits(atlas, "schaefer")) {
    name <- tryCatch(as.character(atlas$name), error = function(e) character(0))
    if (length(name) > 0) {
      name <- name[[1]]
    } else {
      name <- NA_character_
    }
    parcels <- NULL
    networks <- NULL

    if (!is.na(name) &&
        nzchar(name) &&
        grepl("^Schaefer-\\d+-\\d+networks", name, perl = TRUE)) {
      parcels <- suppressWarnings(
        as.integer(sub("^Schaefer-(\\d+)-.*$", "\\1", name))
      )
      networks <- suppressWarnings(
        as.integer(sub("^Schaefer-\\d+-(\\d+)networks.*$", "\\1", name))
      )
    }

    networks <- suppressWarnings(as.integer(networks))
    parcels <- suppressWarnings(as.integer(parcels))
    if (length(networks) != 1) networks <- NA_integer_
    if (length(parcels) != 1) parcels <- NA_integer_

    if (is.na(networks)) {
      if (!is.null(atlas$network)) {
        nunique <- unique(as.character(atlas$network))
        nunique <- nunique[!is.na(nunique) & nzchar(nunique)]
        if (length(nunique) %in% c(7L, 17L)) {
          networks <- length(nunique)
        }
      }
    }

    if (is.na(parcels) &&
        !is.null(atlas$ids) && length(atlas$ids) > 0) {
      n_ids <- length(atlas$ids)
      parcels <- as.integer(n_ids)
    }

    if (!is.na(parcels) && !is.na(networks) &&
        parcels %in% c(100L, 200L, 300L, 400L, 500L, 600L, 800L, 1000L) &&
        networks %in% c(7L, 17L)) {
      return(tryCatch(
        schaefer_surf(
          parcels = parcels,
          networks = networks,
          surf = "inflated"
        ),
        error = function(e) NULL
      ))
    }
  }

  if (inherits(atlas, "glasser")) {
    return(tryCatch(
      glasser_surf(space = "fsaverage", surf = "pial"),
      error = function(e) NULL
    ))
  }

  NULL
}

.set_atlas_volume <- function(atlas, vol) {
  has_atlas <- !is.null(atlas$atlas) &&
    (methods::is(atlas$atlas, "NeuroVol") ||
       methods::is(atlas$atlas, "ClusteredNeuroVol"))
  has_data <- !is.null(atlas$data) &&
    (methods::is(atlas$data, "NeuroVol") ||
       methods::is(atlas$data, "ClusteredNeuroVol"))

  if (isTRUE(has_atlas)) {
    atlas$atlas <- vol
    return(atlas)
  }
  if (isTRUE(has_data)) {
    atlas$data <- vol
    return(atlas)
  }

  atlas$atlas <- vol
  atlas
}

.count_nonzero_voxels <- function(vol) {
  if (methods::is(vol, "ClusteredNeuroVol")) {
    return(length(vol@clusters))
  }
  if (methods::is(vol, "NeuroVol")) {
    arr <- as.array(vol)
    return(sum(is.finite(arr) & arr != 0, na.rm = TRUE))
  }
  NA_integer_
}

.harmonize_cluster_explorer_atlas <- function(atlas, stat_map) {
  ret <- list(atlas = atlas, resampled = FALSE, message = NULL, warning = NULL)

  if (is.null(atlas) || is.null(stat_map) || !inherits(atlas, "atlas")) {
    return(ret)
  }
  if (!methods::is(stat_map, "NeuroVol")) {
    return(ret)
  }

  atlas_vol <- tryCatch(.get_atlas_volume(atlas), error = function(e) NULL)
  if (is.null(atlas_vol)) {
    return(ret)
  }

  atlas_dims <- dim(atlas_vol)[1:3]
  stat_dims <- dim(stat_map)[1:3]
  if (all(atlas_dims == stat_dims)) {
    return(ret)
  }

  target_space <- tryCatch(neuroim2::space(stat_map), error = function(e) NULL)
  if (is.null(target_space) || !methods::is(target_space, "NeuroSpace")) {
    ret$warning <- paste0(
      "Spatial dimensions of atlas (", paste(atlas_dims, collapse = "x"),
      ") and stat_map (", paste(stat_dims, collapse = "x"),
      ") differ, and stat_map space is unavailable for atlas auto-resampling."
    )
    return(ret)
  }

  resampled <- tryCatch(
    resample(atlas_vol, outspace = target_space, smooth = FALSE, interp = 0),
    error = function(e) e
  )
  if (inherits(resampled, "error")) {
    ret$warning <- paste0(
      "Failed to auto-resample atlas from ", paste(atlas_dims, collapse = "x"),
      " to ", paste(stat_dims, collapse = "x"), ": ",
      conditionMessage(resampled)
    )
    return(ret)
  }

  n_nonzero <- .count_nonzero_voxels(resampled)
  if (!is.finite(n_nonzero) || n_nonzero <= 0) {
    ret$warning <- paste0(
      "Atlas auto-resampling produced an empty atlas in stat_map space ",
      "(", paste(stat_dims, collapse = "x"), ")."
    )
    return(ret)
  }

  ret$atlas <- .set_atlas_volume(atlas, resampled)
  ret$resampled <- TRUE
  ret$message <- paste0(
    "cluster_explorer(): resampled atlas from ",
    paste(atlas_dims, collapse = "x"),
    " to ",
    paste(dim(resampled)[1:3], collapse = "x"),
    " to match stat_map space."
  )
  ret
}

.atlas_volume_array <- function(vol) {
  if (methods::is(vol, "ClusteredNeuroVol")) {
    arr <- array(0L, dim = dim(vol))
    arr[which(vol@mask)] <- as.integer(vol@clusters)
    return(arr)
  }
  if (methods::is(vol, "NeuroVol")) {
    arr <- as.array(vol)
    dim(arr) <- dim(vol)[1:3]
    storage.mode(arr) <- "integer"
    return(arr)
  }
  stop("Unsupported volume class for atlas conversion.")
}

.grid_to_linear_index <- function(grid_xyz, dims3) {
  stopifnot(ncol(grid_xyz) == 3)
  x <- as.integer(grid_xyz[, 1])
  y <- as.integer(grid_xyz[, 2])
  z <- as.integer(grid_xyz[, 3])
  as.integer(x + (y - 1L) * dims3[1] + (z - 1L) * dims3[1] * dims3[2])
}
