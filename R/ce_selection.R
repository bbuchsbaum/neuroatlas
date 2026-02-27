.compute_selection_data <- function(selection_engine = c("cluster",
                                                       "parcel",
                                                       "sphere",
                                                       "custom"),
                                   selection_provider = NULL,
                                   data_source,
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
                                   series_cache_env = NULL,
                                   parcel_ids = NULL,
                                   sphere_centers = NULL,
                                   sphere_radius = 6,
                                   sphere_units = c("mm", "voxels"),
                                   sphere_combine = c("separate", "union")) {
  selection_engine <- match.arg(selection_engine)
  connectivity <- match.arg(connectivity)
  tail <- match.arg(tail)
  sphere_units <- match.arg(sphere_units)
  sphere_combine <- match.arg(sphere_combine)

  if (!identical(selection_engine, "custom")) {
    aligned <- .harmonize_cluster_explorer_atlas(atlas, stat_map)
    atlas <- aligned$atlas
    if (!is.null(aligned$message)) {
      message(aligned$message)
    }
    if (!is.null(aligned$warning)) {
      warning(aligned$warning, call. = FALSE)
    }
  }

  if (identical(selection_engine, "cluster")) {
    return(build_cluster_explorer_data(
      data_source = data_source,
      atlas = atlas,
      stat_map = stat_map,
      sample_table = sample_table,
      threshold = threshold,
      min_cluster_size = min_cluster_size,
      connectivity = connectivity,
      tail = tail,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_fun = series_fun,
      prefetch = prefetch,
      prefetch_max_clusters = prefetch_max_clusters,
      prefetch_max_voxels = prefetch_max_voxels,
      series_cache_env = series_cache_env
    ))
  }

  if (identical(selection_engine, "parcel")) {
    return(.build_parcel_selection_data(
      data_source = data_source,
      atlas = atlas,
      stat_map = stat_map,
      sample_table = sample_table,
      parcel_ids = parcel_ids,
      threshold = threshold,
      min_cluster_size = min_cluster_size,
      tail = tail,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_fun = series_fun,
      prefetch = prefetch,
      prefetch_max_clusters = prefetch_max_clusters,
      prefetch_max_voxels = prefetch_max_voxels,
      series_cache_env = series_cache_env
    ))
  }

  if (identical(selection_engine, "sphere")) {
    return(.build_sphere_selection_data(
      data_source = data_source,
      atlas = atlas,
      stat_map = stat_map,
      sample_table = sample_table,
      sphere_centers = sphere_centers,
      sphere_radius = sphere_radius,
      sphere_units = sphere_units,
      sphere_combine = sphere_combine,
      threshold = threshold,
      min_cluster_size = min_cluster_size,
      tail = tail,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_fun = series_fun,
      prefetch = prefetch,
      prefetch_max_clusters = prefetch_max_clusters,
      prefetch_max_voxels = prefetch_max_voxels,
      series_cache_env = series_cache_env
    ))
  }

  if (!is.function(selection_provider)) {
    stop(
      "selection_provider must be a function when selection_engine = 'custom'.",
      call. = FALSE
    )
  }

  out <- selection_provider(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    threshold = threshold,
    min_cluster_size = min_cluster_size,
    connectivity = connectivity,
    tail = tail,
    signal_fun = signal_fun,
    signal_fun_args = signal_fun_args,
    series_fun = series_fun,
    prefetch = prefetch,
    prefetch_max_clusters = prefetch_max_clusters,
    prefetch_max_voxels = prefetch_max_voxels,
    series_cache_env = series_cache_env,
    parcel_ids = parcel_ids,
    sphere_centers = sphere_centers,
    sphere_radius = sphere_radius,
    sphere_units = sphere_units,
    sphere_combine = sphere_combine
  )
  required <- c(
    "cluster_table", "cluster_parcels", "cluster_ts", "cluster_voxels",
    "cluster_index", "sample_table", "prefetch_info"
  )
  miss <- setdiff(required, names(out))
  if (length(miss) > 0) {
    stop(
      "selection_provider result missing required fields: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  out
}

.build_parcel_selection_data <- function(data_source,
                                         atlas,
                                         stat_map,
                                         sample_table = NULL,
                                         parcel_ids = NULL,
                                         threshold = 3,
                                         min_cluster_size = 20,
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
  tail <- match.arg(tail)
  atlas_arr <- .atlas_volume_array(.get_atlas_volume(atlas))
  stat_arr <- as.array(stat_map)

  ids_all <- sort(unique(as.integer(atlas_arr[atlas_arr != 0])))
  if (is.null(parcel_ids)) {
    ids <- ids_all
  } else {
    ids <- intersect(ids_all, as.integer(parcel_ids))
  }

  cluster_voxels <- list()
  cluster_sign <- character(0)
  for (pid in ids) {
    idx <- which(atlas_arr == pid)
    if (length(idx) == 0) next

    vox <- as.matrix(arrayInd(idx, .dim = dim(atlas_arr)))
    if (nrow(vox) < min_cluster_size) next

    svals <- as.numeric(stat_arr[idx])
    peak <- svals[which.max(abs(svals))]
    if (!.keep_peak_for_tail(peak_stat = peak, threshold = threshold, tail = tail)) {
      next
    }

    cid <- paste0("R", pid)
    cluster_voxels[[cid]] <- vox
    cluster_sign[[cid]] <- .sign_label_from_peak(peak)
  }

  .build_selection_data_from_voxels(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    cluster_voxels = cluster_voxels,
    cluster_sign = cluster_sign,
    signal_fun = signal_fun,
    signal_fun_args = signal_fun_args,
    series_fun = series_fun,
    prefetch = prefetch,
    prefetch_max_clusters = prefetch_max_clusters,
    prefetch_max_voxels = prefetch_max_voxels,
    series_cache_env = series_cache_env
  )
}

.build_sphere_selection_data <- function(data_source,
                                         atlas,
                                         stat_map,
                                         sample_table = NULL,
                                         sphere_centers = NULL,
                                         sphere_radius = 6,
                                         sphere_units = c("mm", "voxels"),
                                         sphere_combine = c("separate", "union"),
                                         threshold = 3,
                                         min_cluster_size = 20,
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
  tail <- match.arg(tail)
  sphere_units <- match.arg(sphere_units)
  sphere_combine <- match.arg(sphere_combine)

  assertthat::assert_that(
    is.numeric(sphere_radius) && length(sphere_radius) == 1 &&
      is.finite(sphere_radius) && sphere_radius > 0,
    msg = "'sphere_radius' must be a positive numeric scalar."
  )

  dims3 <- dim(stat_map)[1:3]
  stat_arr <- as.array(stat_map)
  all_idx <- seq_len(prod(dims3))
  grid_all <- as.matrix(arrayInd(all_idx, .dim = dims3))
  coord_all <- if (identical(sphere_units, "mm")) {
    neuroim2::grid_to_coord(neuroim2::space(stat_map), grid_all)
  } else {
    grid_all
  }

  centers <- .normalize_sphere_centers(sphere_centers)
  if (is.null(centers)) {
    peak_idx <- which.max(abs(stat_arr))
    peak_grid <- as.matrix(arrayInd(peak_idx, .dim = dims3))
    centers <- if (identical(sphere_units, "mm")) {
      neuroim2::grid_to_coord(neuroim2::space(stat_map), peak_grid)
    } else {
      peak_grid
    }
  }

  cluster_voxels <- list()
  cluster_sign <- character(0)

  if (identical(sphere_combine, "union")) {
    keep <- rep(FALSE, nrow(coord_all))
    for (i in seq_len(nrow(centers))) {
      d2 <- rowSums((coord_all - matrix(centers[i, ], nrow(coord_all), 3,
                                        byrow = TRUE))^2)
      keep <- keep | (d2 <= sphere_radius^2)
    }
    idx <- which(keep)
    if (length(idx) > 0) {
      vox <- grid_all[idx, , drop = FALSE]
      svals <- as.numeric(stat_arr[idx])
      peak <- svals[which.max(abs(svals))]
      if (nrow(vox) >= min_cluster_size &&
          .keep_peak_for_tail(peak_stat = peak, threshold = threshold, tail = tail)) {
        cluster_voxels[["S1"]] <- vox
        cluster_sign[["S1"]] <- .sign_label_from_peak(peak)
      }
    }
  } else {
    for (i in seq_len(nrow(centers))) {
      d2 <- rowSums((coord_all - matrix(centers[i, ], nrow(coord_all), 3,
                                        byrow = TRUE))^2)
      idx <- which(d2 <= sphere_radius^2)
      if (length(idx) == 0) next
      vox <- grid_all[idx, , drop = FALSE]
      if (nrow(vox) < min_cluster_size) next

      svals <- as.numeric(stat_arr[idx])
      peak <- svals[which.max(abs(svals))]
      if (!.keep_peak_for_tail(peak_stat = peak, threshold = threshold, tail = tail)) {
        next
      }
      cid <- paste0("S", i)
      cluster_voxels[[cid]] <- vox
      cluster_sign[[cid]] <- .sign_label_from_peak(peak)
    }
  }

  .build_selection_data_from_voxels(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    cluster_voxels = cluster_voxels,
    cluster_sign = cluster_sign,
    signal_fun = signal_fun,
    signal_fun_args = signal_fun_args,
    series_fun = series_fun,
    prefetch = prefetch,
    prefetch_max_clusters = prefetch_max_clusters,
    prefetch_max_voxels = prefetch_max_voxels,
    series_cache_env = series_cache_env
  )
}

.build_selection_data_from_voxels <- function(data_source,
                                              atlas,
                                              stat_map,
                                              sample_table = NULL,
                                              cluster_voxels,
                                              cluster_sign = character(0),
                                              signal_fun = mean,
                                              signal_fun_args = list(na.rm = TRUE),
                                              series_fun = NULL,
                                              prefetch = TRUE,
                                              prefetch_max_clusters = Inf,
                                              prefetch_max_voxels = Inf,
                                              series_cache_env = NULL) {
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
  sample_tbl <- .normalize_sample_table(sample_table = sample_table,
                                        n_samples = n_samples)

  stat_arr <- as.array(stat_map)
  dims3 <- dim(stat_arr)[1:3]
  index_arr <- array(0L, dim = dims3)

  rows <- list()
  clean_vox <- list()
  k <- 0L
  ids <- names(cluster_voxels)
  if (is.null(ids)) ids <- paste0("K", seq_along(cluster_voxels))

  for (i in seq_along(cluster_voxels)) {
    vox <- as.matrix(cluster_voxels[[i]])
    if (is.null(vox) || nrow(vox) == 0) next
    if (ncol(vox) != 3) next

    vox <- matrix(as.integer(vox), ncol = 3)
    vox <- vox[
      vox[, 1] >= 1 & vox[, 1] <= dims3[1] &
        vox[, 2] >= 1 & vox[, 2] <= dims3[2] &
        vox[, 3] >= 1 & vox[, 3] <= dims3[3],
      , drop = FALSE
    ]
    if (nrow(vox) == 0) next
    vox <- unique(vox)

    idx <- .grid_to_linear_index(vox, dims3)
    svals <- as.numeric(stat_arr[idx])
    peak_i <- which.max(abs(svals))
    peak <- vox[peak_i, ]
    peak_stat <- svals[peak_i]

    k <- k + 1L
    cid <- as.character(ids[i])
    clean_vox[[cid]] <- vox
    index_arr[idx] <- k

    sign_val <- cluster_sign[[cid]]
    if (is.null(sign_val) || !is.character(sign_val) ||
        length(sign_val) == 0 || is.na(sign_val) || !nzchar(sign_val[1])) {
      sign_val <- .sign_label_from_peak(peak_stat)
    }

    rows[[length(rows) + 1L]] <- tibble::tibble(
      cluster_id = cid,
      sign = sign_val,
      component_id = k,
      n_voxels = nrow(vox),
      peak_x = as.integer(peak[1]),
      peak_y = as.integer(peak[2]),
      peak_z = as.integer(peak[3]),
      max_stat = as.numeric(peak_stat),
      peak_coord = paste0("(", peak[1], ", ", peak[2], ", ", peak[3], ")")
    )
  }

  cluster_tbl <- if (length(rows) == 0) {
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
    dplyr::bind_rows(rows)
  }

  ann <- .annotate_clusters_with_atlas(
    cluster_table = cluster_tbl,
    cluster_voxels = clean_vox,
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
      cluster_voxels = clean_vox,
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
    cluster_voxels = clean_vox,
    cluster_index = neuroim2::NeuroVol(index_arr, space = neuroim2::space(stat_map)),
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

.normalize_sphere_centers <- function(sphere_centers) {
  if (is.null(sphere_centers)) return(NULL)
  if (is.numeric(sphere_centers) && length(sphere_centers) == 3) {
    return(matrix(as.numeric(sphere_centers), nrow = 1))
  }
  if (is.data.frame(sphere_centers)) {
    sphere_centers <- as.matrix(sphere_centers)
  }
  if (is.matrix(sphere_centers) && ncol(sphere_centers) == 3) {
    storage.mode(sphere_centers) <- "double"
    return(sphere_centers)
  }
  stop(
    "'sphere_centers' must be NULL, a numeric length-3 vector, ",
    "or an n x 3 matrix/data.frame.",
    call. = FALSE
  )
}

.keep_peak_for_tail <- function(peak_stat, threshold, tail) {
  if (!is.finite(peak_stat)) return(FALSE)
  if (identical(tail, "positive")) {
    return(peak_stat > threshold)
  }
  if (identical(tail, "negative")) {
    return(peak_stat < -threshold)
  }
  abs(peak_stat) > threshold
}

.sign_label_from_peak <- function(peak_stat) {
  if (!is.finite(peak_stat) || peak_stat == 0) return("unsigned")
  if (peak_stat > 0) "positive" else "negative"
}
