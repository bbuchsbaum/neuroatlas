utils::globalVariables(c("signal", "cluster_id"))

#' Build Cluster Explorer Data
#'
#' Compute sign-aware volumetric connected components from a statistic map,
#' annotate them with atlas parcels, and extract cluster-level 4D signal
#' summaries aligned to sample rows.
#'
#' @param data_source A sample-wise data source supporting
#'   \code{neuroim2::series(data_source, i)} where \code{i} is voxel coordinates
#'   (\code{n x 3} matrix) or indices. Rows of the returned matrix correspond to
#'   samples/design rows.
#' @param atlas A volumetric \code{atlas} object used for parcel annotation.
#'   When atlas and \code{stat_map} dimensions differ, the atlas is
#'   automatically resampled to \code{stat_map} space (nearest-neighbor labels)
#'   before cluster annotation.
#' @param stat_map A \code{NeuroVol} statistic image used for thresholding and
#'   clustering.
#' @param sample_table Optional data frame with one row per sample. If
#'   \code{NULL}, a default table with \code{.sample_index} is created.
#' @param threshold Numeric threshold used for cluster formation.
#' @param min_cluster_size Minimum number of voxels required to keep a cluster.
#' @param connectivity Connectivity passed to \code{neuroim2::conn_comp()}.
#' @param tail Clustering mode: \code{"positive"}, \code{"negative"}, or
#'   \code{"two_sided"}.
#' @param signal_fun Function used to summarize cluster signal per sample.
#' @param signal_fun_args Named list of additional arguments passed to
#'   \code{signal_fun}.
#' @param series_fun Optional function override for extracting voxel-wise sample
#'   series. Must accept \code{(data_source, i)} and return a matrix-like object
#'   with one row per sample.
#' @param prefetch Logical; if \code{TRUE}, precompute signal summaries for all
#'   clusters on recompute.
#' @param prefetch_max_clusters Maximum clusters allowed for eager prefetch.
#'   Prefetch is skipped when exceeded.
#' @param prefetch_max_voxels Maximum total cluster voxels allowed for eager
#'   prefetch. Prefetch is skipped when exceeded.
#' @param series_cache_env Optional environment used to memoize
#'   voxel-coordinates-to-series extraction across cluster computations.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{cluster_table}}{Cluster summary tibble.}
#'   \item{\code{cluster_parcels}}{Cluster-parcel overlap tibble.}
#'   \item{\code{cluster_ts}}{Sample-level cluster signal tibble.}
#'   \item{\code{cluster_voxels}}{Named list of voxel coordinate matrices by
#'     cluster ID.}
#'   \item{\code{cluster_index}}{A \code{NeuroVol} of global cluster IDs.}
#'   \item{\code{sample_table}}{Normalized sample table with
#'     \code{.sample_index}.}
#'   \item{\code{prefetch_info}}{List describing whether eager signal prefetch
#'     was requested/applied and the effective guard thresholds.}
#' }
#' @export
build_cluster_explorer_data <- function(data_source,
                                        atlas,
                                        stat_map,
                                        sample_table = NULL,
                                        threshold = 3,
                                        min_cluster_size = 20,
                                        connectivity = c("26-connect",
                                                         "18-connect",
                                                         "6-connect"),
                                        tail = c("two_sided",
                                                 "positive",
                                                 "negative"),
                                        signal_fun = mean,
                                        signal_fun_args = list(na.rm = TRUE),
                                        series_fun = NULL,
                                        prefetch = TRUE,
                                        prefetch_max_clusters = Inf,
                                        prefetch_max_voxels = Inf,
                                        series_cache_env = NULL) {
  connectivity <- match.arg(connectivity)
  tail <- match.arg(tail)

  aligned <- .harmonize_cluster_explorer_atlas(atlas, stat_map)
  atlas <- aligned$atlas
  if (!is.null(aligned$message)) {
    message(aligned$message)
  }
  if (!is.null(aligned$warning)) {
    warning(aligned$warning, call. = FALSE)
  }

  n_samples <- .infer_n_samples(
    data_source = data_source,
    sample_table = sample_table,
    design = NULL
  )

  .validate_cluster_explorer_inputs(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    n_samples = n_samples,
    series_fun = series_fun
  )

  sample_tbl <- .normalize_sample_table(
    sample_table = sample_table,
    n_samples = n_samples
  )

  comp <- .extract_stat_clusters(
    stat_map = stat_map,
    threshold = threshold,
    min_cluster_size = min_cluster_size,
    connectivity = connectivity,
    tail = tail
  )

  ann <- .annotate_clusters_with_atlas(
    cluster_table = comp$cluster_table,
    cluster_voxels = comp$cluster_voxels,
    atlas = atlas,
    stat_map = stat_map
  )

  n_clusters <- nrow(ann$cluster_table)
  total_voxels <- if (n_clusters > 0) sum(ann$cluster_table$n_voxels) else 0
  prefetch_allowed <- isTRUE(prefetch) &&
    (n_clusters <= prefetch_max_clusters) &&
    (total_voxels <= prefetch_max_voxels)

  if (isTRUE(prefetch_allowed)) {
    ts_tbl <- .compute_cluster_timeseries(
      data_source = data_source,
      cluster_voxels = comp$cluster_voxels,
      sample_table = sample_tbl,
      series_fun = series_fun,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_cache_env = series_cache_env
    )
  } else {
    ts_tbl <- tibble::tibble()
  }

  if (nrow(ts_tbl) > 0 && nrow(ann$cluster_table) > 0) {
    ts_tbl <- dplyr::left_join(
      ts_tbl,
      ann$cluster_table[, c("cluster_id", "sign"), drop = FALSE],
      by = "cluster_id"
    )
  }

  list(
    cluster_table = ann$cluster_table,
    cluster_parcels = ann$cluster_parcels,
    cluster_ts = ts_tbl,
    cluster_voxels = comp$cluster_voxels,
    cluster_index = comp$cluster_index,
    sample_table = sample_tbl,
    prefetch_info = list(
      requested = isTRUE(prefetch),
      applied = isTRUE(prefetch_allowed),
      n_clusters = n_clusters,
      total_voxels = total_voxels,
      max_clusters = prefetch_max_clusters,
      max_voxels = prefetch_max_voxels
    )
  )
}

.infer_n_samples <- function(data_source, sample_table = NULL, design = NULL) {
  ds_dim <- dim(data_source)
  if (!is.null(ds_dim)) {
    if (length(ds_dim) >= 4) {
      return(as.integer(ds_dim[4]))
    }
    if (length(ds_dim) == 3) {
      return(1L)
    }
  }
  if (!is.null(sample_table)) {
    return(nrow(sample_table))
  }
  if (!is.null(design)) {
    return(nrow(design))
  }
  stop(
    "Unable to infer number of samples from data_source. ",
    "Provide a sample_table or design with one row per sample."
  )
}

.normalize_sample_table <- function(sample_table, n_samples) {
  if (is.null(sample_table)) {
    tbl <- tibble::tibble(.sample_index = seq_len(n_samples))
  } else {
    tbl <- tibble::as_tibble(sample_table, .name_repair = "unique")
    tbl$.sample_index <- seq_len(n_samples)
  }
  tbl
}

.normalize_design_table <- function(design, n_samples) {
  if (is.null(design)) {
    return(NULL)
  }

  assertthat::assert_that(
    is.data.frame(design),
    msg = "'design' must be a data.frame when provided."
  )
  assertthat::assert_that(
    nrow(design) == n_samples,
    msg = paste0(
      "nrow(design) (", nrow(design),
      ") must equal number of samples (", n_samples, ")."
    )
  )

  tibble::as_tibble(design, .name_repair = "unique")
}

.merge_sample_and_design <- function(sample_table, design_table) {
  if (is.null(design_table)) {
    return(sample_table)
  }

  # Keep sample index from sample_table as the canonical join row order.
  design_table$.sample_index <- sample_table$.sample_index
  dplyr::left_join(sample_table, design_table, by = ".sample_index",
                   suffix = c("", ".design"))
}

.validate_cluster_explorer_inputs <- function(data_source,
                                              atlas,
                                              stat_map,
                                              sample_table = NULL,
                                              n_samples,
                                              series_fun = NULL) {
  assertthat::assert_that(
    inherits(atlas, "atlas"),
    msg = "'atlas' must inherit from class 'atlas'."
  )
  assertthat::assert_that(
    methods::is(stat_map, "NeuroVol"),
    msg = "'stat_map' must be a NeuroVol."
  )

  atlas_vol <- .get_atlas_volume(atlas)
  atlas_dims <- dim(atlas_vol)[1:3]
  data_dims <- dim(data_source)
  if (!is.null(data_dims) && length(data_dims) >= 3) {
    data_dims <- data_dims[1:3]
  } else {
    data_dims <- NULL
  }
  stat_dims <- dim(stat_map)[1:3]

  if (!is.null(data_dims)) {
    assertthat::assert_that(
      all(stat_dims == data_dims),
      msg = paste0(
        "Spatial dimensions of stat_map (",
        paste(stat_dims, collapse = "x"),
        ") and data_source (",
        paste(data_dims, collapse = "x"),
        ") must match when data_source has spatial dims."
      )
    )
  }
  assertthat::assert_that(
    all(atlas_dims == stat_dims),
    msg = paste0(
      "Spatial dimensions of atlas (",
      paste(atlas_dims, collapse = "x"),
      ") and stat_map (",
      paste(stat_dims, collapse = "x"),
      ") must match."
    )
  )

  if (!is.null(sample_table)) {
    assertthat::assert_that(
      is.data.frame(sample_table),
      msg = "'sample_table' must be a data.frame when provided."
    )
    assertthat::assert_that(
      nrow(sample_table) == n_samples,
      msg = paste0(
        "nrow(sample_table) (", nrow(sample_table),
        ") must equal number of samples (", n_samples, ")."
      )
    )
  }

  if (!is.null(series_fun)) {
    assertthat::assert_that(
      is.function(series_fun),
      msg = "'series_fun' must be a function(data_source, i)."
    )
  }

  invisible(TRUE)
}

.extract_stat_clusters <- function(stat_map,
                                   threshold,
                                   min_cluster_size,
                                   connectivity,
                                   tail) {
  stat_arr <- as.array(stat_map)
  if (length(dim(stat_arr)) != 3) {
    stop("'stat_map' must be a 3D NeuroVol.")
  }

  out_tbl <- list()
  cluster_voxels <- list()
  cluster_arr <- array(0L, dim = dim(stat_arr))
  global_id <- 0L

  tails <- switch(
    tail,
    positive = "positive",
    negative = "negative",
    two_sided = c("positive", "negative")
  )

  for (sgn in tails) {
    if (sgn == "positive") {
      work_arr <- stat_arr
      mask <- stat_arr > threshold
      prefix <- "P"
    } else {
      work_arr <- -stat_arr
      mask <- stat_arr < -threshold
      prefix <- "N"
    }

    if (!any(mask, na.rm = TRUE)) {
      next
    }

    work_vol <- neuroim2::NeuroVol(work_arr, space = neuroim2::space(stat_map))
    cc <- neuroim2::conn_comp(
      work_vol,
      threshold = threshold,
      cluster_table = TRUE,
      local_maxima = FALSE,
      connect = connectivity
    )

    if (is.null(cc$cluster_table) || nrow(cc$cluster_table) == 0) {
      next
    }

    ct <- cc$cluster_table
    ct <- ct[ct$N >= min_cluster_size, , drop = FALSE]
    if (nrow(ct) == 0) {
      next
    }

    for (i in seq_len(nrow(ct))) {
      local_idx <- as.integer(ct$index[i])
      vox <- cc$voxels[[as.character(local_idx)]]
      if (is.null(vox) || nrow(vox) == 0) {
        next
      }

      vox_mat <- as.matrix(vox[, c("x", "y", "z"), drop = FALSE])
      peak_xyz <- c(as.integer(ct$x[i]),
                    as.integer(ct$y[i]),
                    as.integer(ct$z[i]))
      peak_stat <- stat_arr[matrix(peak_xyz, nrow = 1)]

      global_id <- global_id + 1L
      cluster_id <- paste0(prefix, global_id)

      cluster_voxels[[cluster_id]] <- vox_mat
      idx_lin <- .grid_to_linear_index(vox_mat, dim(cluster_arr))
      cluster_arr[idx_lin] <- global_id

      out_tbl[[length(out_tbl) + 1L]] <- tibble::tibble(
        cluster_id = cluster_id,
        sign = sgn,
        component_id = local_idx,
        n_voxels = as.integer(ct$N[i]),
        peak_x = peak_xyz[1],
        peak_y = peak_xyz[2],
        peak_z = peak_xyz[3],
        max_stat = as.numeric(peak_stat),
        peak_coord = paste0("(", peak_xyz[1], ", ", peak_xyz[2], ", ",
                            peak_xyz[3], ")")
      )
    }
  }

  cluster_tbl <- if (length(out_tbl) == 0) {
    tibble::tibble(
      cluster_id = character(0),
      sign = character(0),
      component_id = integer(0),
      n_voxels = integer(0),
      peak_x = integer(0),
      peak_y = integer(0),
      peak_z = integer(0),
      max_stat = numeric(0),
      peak_coord = character(0)
    )
  } else {
    dplyr::bind_rows(out_tbl)
  }

  list(
    cluster_table = cluster_tbl,
    cluster_voxels = cluster_voxels,
    cluster_index = neuroim2::NeuroVol(cluster_arr, space = neuroim2::space(stat_map))
  )
}

.annotate_clusters_with_atlas <- function(cluster_table,
                                          cluster_voxels,
                                          atlas,
                                          stat_map) {
  if (nrow(cluster_table) == 0 || length(cluster_voxels) == 0) {
    cluster_table$atlas_label_primary <- character(0)
    cluster_table$n_parcels <- integer(0)
    cluster_table$parcel_overlap <- numeric(0)
    return(list(
      cluster_table = cluster_table,
      cluster_parcels = tibble::tibble(
        cluster_id = character(0),
        sign = character(0),
        parcel_id = integer(0),
        parcel_label = character(0),
        n_voxels = integer(0),
        frac = numeric(0),
        peak_stat = numeric(0),
        max_pos = numeric(0),
        min_neg = numeric(0)
      )
    ))
  }

  atlas_arr <- .atlas_volume_array(.get_atlas_volume(atlas))
  stat_arr <- as.array(stat_map)
  meta <- roi_metadata(atlas)
  label_map <- stats::setNames(as.character(meta$label), as.character(meta$id))

  tbl <- cluster_table
  tbl$atlas_label_primary <- NA_character_
  tbl$n_parcels <- 0L
  tbl$parcel_overlap <- 0

  parcel_rows <- list()

  for (i in seq_len(nrow(tbl))) {
    cid <- tbl$cluster_id[i]
    vox <- cluster_voxels[[cid]]
    if (is.null(vox) || nrow(vox) == 0) {
      next
    }

    idx_lin <- .grid_to_linear_index(vox, dim(atlas_arr))
    parcel_ids <- atlas_arr[idx_lin]
    stat_vals <- stat_arr[idx_lin]
    keep <- !is.na(parcel_ids) & parcel_ids != 0
    parcel_ids <- as.integer(parcel_ids[keep])
    stat_vals <- as.numeric(stat_vals[keep])

    if (length(parcel_ids) == 0) {
      next
    }

    tab <- sort(table(parcel_ids), decreasing = TRUE)
    pids <- as.integer(names(tab))
    pcounts <- as.integer(tab)
    pfrac <- pcounts / sum(pcounts)

    tbl$atlas_label_primary[i] <- label_map[as.character(pids[1])]
    tbl$n_parcels[i] <- length(pids)
    tbl$parcel_overlap[i] <- pfrac[1]

    for (j in seq_along(pids)) {
      pid <- pids[j]
      svals <- stat_vals[parcel_ids == pid]
      svals <- as.numeric(svals)
      finite <- is.finite(svals)
      svals_finite <- svals[finite]

      peak_stat <- if (length(svals_finite) == 0) {
        NA_real_
      } else {
        svals_finite[which.max(abs(svals_finite))]
      }
      max_pos <- if (any(svals_finite > 0, na.rm = TRUE)) {
        max(svals_finite[svals_finite > 0], na.rm = TRUE)
      } else {
        NA_real_
      }
      min_neg <- if (any(svals_finite < 0, na.rm = TRUE)) {
        min(svals_finite[svals_finite < 0], na.rm = TRUE)
      } else {
        NA_real_
      }

      parcel_rows[[length(parcel_rows) + 1L]] <- tibble::tibble(
        cluster_id = cid,
        sign = tbl$sign[i],
        parcel_id = pid,
        parcel_label = label_map[as.character(pid)],
        n_voxels = pcounts[j],
        frac = pfrac[j],
        peak_stat = peak_stat,
        max_pos = max_pos,
        min_neg = min_neg
      )
    }
  }

  cluster_parcels <- if (length(parcel_rows) == 0) {
    tibble::tibble(
      cluster_id = character(0),
      sign = character(0),
      parcel_id = integer(0),
      parcel_label = character(0),
      n_voxels = integer(0),
      frac = numeric(0),
      peak_stat = numeric(0),
      max_pos = numeric(0),
      min_neg = numeric(0)
    )
  } else {
    dplyr::bind_rows(parcel_rows)
  }

  list(cluster_table = tbl, cluster_parcels = cluster_parcels)
}

.compute_cluster_timeseries <- function(data_source,
                                        cluster_voxels,
                                        sample_table,
                                        series_fun = NULL,
                                        signal_fun = mean,
                                        signal_fun_args = list(na.rm = TRUE),
                                        series_cache_env = NULL) {
  if (length(cluster_voxels) == 0) {
    return(tibble::tibble())
  }

  stopifnot(is.function(signal_fun))
  n_samples <- nrow(sample_table)
  ids <- names(cluster_voxels)
  out <- vector("list", length(ids))
  names(out) <- ids

  for (k in seq_along(ids)) {
    cid <- ids[k]
    vox <- cluster_voxels[[cid]]
    series_mat <- .extract_series_matrix(
      data_source = data_source,
      voxel_coords = vox,
      n_samples = n_samples,
      series_fun = series_fun,
      cache_env = series_cache_env,
      cache_key = cid
    )

    sig <- vapply(seq_len(n_samples), function(t) {
      vals <- series_mat[t, , drop = TRUE]
      do.call(signal_fun, c(list(vals), signal_fun_args))
    }, numeric(1))

    out[[k]] <- tibble::tibble(
      .sample_index = seq_len(n_samples),
      cluster_id = cid,
      signal = sig
    )
  }

  ret <- dplyr::bind_rows(out)
  dplyr::left_join(ret, sample_table, by = ".sample_index")
}

.extract_series_matrix <- function(data_source,
                                   voxel_coords,
                                   n_samples,
                                   series_fun = NULL,
                                   cache_env = NULL,
                                   cache_key = NULL) {
  if (!is.null(cache_env) && !is.null(cache_key) &&
      exists(cache_key, envir = cache_env, inherits = FALSE)) {
    cached <- get(cache_key, envir = cache_env, inherits = FALSE)
    return(.coerce_series_matrix(cached, n_samples = n_samples))
  }

  raw <- tryCatch(
    {
      if (!is.null(series_fun)) {
        series_fun(data_source, voxel_coords)
      } else if (methods::is(data_source, "NeuroVol")) {
        matrix(data_source[voxel_coords], nrow = 1)
      } else {
        neuroim2::series(data_source, voxel_coords)
      }
    },
    error = function(e) {
      stop(
        "Failed to extract sample series from data_source. ",
        "Ensure neuroim2::series(data_source, voxel_coords) works, ",
        "or supply a compatible series_fun(data_source, voxel_coords). ",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  out <- .coerce_series_matrix(raw, n_samples = n_samples)
  if (!is.null(cache_env) && !is.null(cache_key)) {
    assign(cache_key, out, envir = cache_env)
  }
  out
}

.coerce_series_matrix <- function(x, n_samples) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (is.null(dim(x))) {
    x <- matrix(as.numeric(x), ncol = 1)
  } else {
    x <- as.matrix(x)
  }

  if (nrow(x) == n_samples) {
    return(x)
  }
  if (ncol(x) == n_samples) {
    return(t(x))
  }
  if (length(x) == n_samples) {
    return(matrix(as.numeric(x), ncol = 1))
  }

  stop(
    "Extracted series has incompatible shape. Expected ", n_samples,
    " sample rows, got matrix ", nrow(x), "x", ncol(x), ".",
    call. = FALSE
  )
}

.resolve_cluster_ts <- function(dat,
                                selected_cluster_ids,
                                data_source,
                                series_fun,
                                signal_fun,
                                signal_fun_args,
                                series_cache_env,
                                ts_cache_env) {
  ids <- unique(as.character(selected_cluster_ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) return(tibble::tibble())

  cached_ids <- ids[vapply(ids, function(cid) {
    exists(cid, envir = ts_cache_env, inherits = FALSE)
  }, logical(1))]
  missing_ids <- setdiff(ids, cached_ids)

  if (length(missing_ids) > 0) {
    missing_vox <- dat$cluster_voxels[missing_ids]
    missing_vox <- missing_vox[!vapply(missing_vox, is.null, logical(1))]
    if (length(missing_vox) > 0) {
      ts_new <- .compute_cluster_timeseries(
        data_source = data_source,
        cluster_voxels = missing_vox,
        sample_table = dat$sample_table,
        series_fun = series_fun,
        signal_fun = signal_fun,
        signal_fun_args = signal_fun_args,
        series_cache_env = series_cache_env
      )
      if (nrow(ts_new) > 0) {
        if (nrow(dat$cluster_table) > 0) {
          ts_new <- dplyr::left_join(
            ts_new,
            dat$cluster_table[, c("cluster_id", "sign"), drop = FALSE],
            by = "cluster_id"
          )
        }
        for (cid in unique(ts_new$cluster_id)) {
          assign(cid,
                 ts_new[ts_new$cluster_id == cid, , drop = FALSE],
                 envir = ts_cache_env)
        }
      }
    }
  }

  out <- lapply(ids, function(cid) {
    if (exists(cid, envir = ts_cache_env, inherits = FALSE)) {
      get(cid, envir = ts_cache_env, inherits = FALSE)
    } else {
      NULL
    }
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(tibble::tibble())
  dplyr::bind_rows(out)
}
