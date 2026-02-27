.clusters_for_parcels <- function(cluster_parcels, parcel_ids) {
  if (nrow(cluster_parcels) == 0 || length(parcel_ids) == 0) {
    return(character(0))
  }

  keep <- cluster_parcels$parcel_id %in% as.integer(parcel_ids)
  rows <- cluster_parcels[keep, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(character(0))
  }

  score <- stats::aggregate(
    rows$frac,
    by = list(cluster_id = rows$cluster_id),
    FUN = sum
  )
  score <- score[order(score$x, decreasing = TRUE), , drop = FALSE]
  as.character(score$cluster_id)
}

.parse_plot_brain_selection_ids <- function(ids) {
  ids <- as.character(ids)
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0) {
    return(tibble::tibble(
      raw_id = character(0),
      panel = character(0),
      parcel_id = integer(0),
      shape_id = integer(0)
    ))
  }

  rows <- lapply(ids, function(id) {
    parts <- strsplit(id, "::", fixed = TRUE)[[1]]
    if (length(parts) == 3) {
      pid <- suppressWarnings(as.integer(parts[2]))
      sid <- suppressWarnings(as.integer(parts[3]))
      if (is.finite(pid) && is.finite(sid)) {
        return(tibble::tibble(
          raw_id = id,
          panel = as.character(parts[1]),
          parcel_id = as.integer(pid),
          shape_id = as.integer(sid)
        ))
      }
    }

    pid <- suppressWarnings(as.integer(id))
    tibble::tibble(
      raw_id = id,
      panel = NA_character_,
      parcel_id = if (is.finite(pid)) as.integer(pid) else NA_integer_,
      shape_id = NA_integer_
    )
  })

  dplyr::bind_rows(rows)
}

.surface_pick_surface_to_world <- function(geometry, surface_xyz) {
  xform <- tryCatch(neurosurf::surf_to_world(geometry), error = function(e) NULL)
  if (is.null(xform) || !is.matrix(xform) || !all(dim(xform) == c(4, 4))) {
    return(as.numeric(surface_xyz))
  }

  v <- matrix(as.numeric(surface_xyz), nrow = 1)
  rot <- xform[1:3, 1:3, drop = FALSE]
  trans <- matrix(xform[1:3, 4], nrow = 1)
  as.numeric(v %*% t(rot) + trans)
}

.surface_pick_round_clip_grid <- function(grid_xyz, dims3) {
  if (is.null(dim(grid_xyz))) {
    g <- as.integer(round(as.numeric(grid_xyz)))
  } else {
    g <- as.integer(round(as.numeric(grid_xyz[1, ])))
  }
  d <- as.integer(dims3)
  pmax(1L, pmin(d, g))
}

.surface_pick_lookup_from_polygons <- function(poly, panel_ctx, stat_map) {
  empty <- tibble::tibble(
    data_id = character(0),
    panel = character(0),
    parcel_id = integer(0),
    shape_id = integer(0),
    vertex_index = integer(0),
    surface_x = numeric(0),
    surface_y = numeric(0),
    surface_z = numeric(0),
    world_x = numeric(0),
    world_y = numeric(0),
    world_z = numeric(0),
    grid_x = integer(0),
    grid_y = integer(0),
    grid_z = integer(0)
  )

  if (is.null(poly) || nrow(poly) == 0) {
    return(empty)
  }

  key <- unique(poly[, c("panel", "parcel_id", "poly_id"), drop = FALSE])
  rows <- vector("list", nrow(key))
  dims3 <- dim(stat_map)[1:3]
  vol_space <- neuroim2::space(stat_map)

  for (i in seq_len(nrow(key))) {
    panel <- as.character(key$panel[i])
    parcel_id <- as.integer(key$parcel_id[i])
    shape_id <- as.integer(key$poly_id[i])
    ctx <- panel_ctx[[panel]]
    if (is.null(ctx)) next

    sub <- poly[
      poly$panel == panel &
        poly$parcel_id == parcel_id &
        poly$poly_id == shape_id,
      ,
      drop = FALSE
    ]
    if (nrow(sub) == 0) next

    centroid <- c(mean(sub$x), mean(sub$y))
    cand <- which(ctx$parcels == parcel_id)
    if (length(cand) == 0) cand <- seq_len(nrow(ctx$xy))
    if (length(cand) == 0) next

    d2 <- (ctx$xy[cand, 1] - centroid[1])^2 +
      (ctx$xy[cand, 2] - centroid[2])^2
    best <- cand[which.min(d2)]
    surface_xyz <- as.numeric(ctx$verts[best, ])
    world_xyz <- .surface_pick_surface_to_world(ctx$geometry, surface_xyz)
    grid_xyz <- neuroim2::coord_to_grid(vol_space, matrix(world_xyz, nrow = 1))
    grid_xyz <- .surface_pick_round_clip_grid(grid_xyz, dims3 = dims3)

    rows[[i]] <- tibble::tibble(
      data_id = .encode_plot_brain_data_id(
        panel = panel,
        parcel_id = parcel_id,
        shape_id = shape_id
      ),
      panel = panel,
      parcel_id = parcel_id,
      shape_id = shape_id,
      vertex_index = as.integer(best),
      surface_x = surface_xyz[1],
      surface_y = surface_xyz[2],
      surface_z = surface_xyz[3],
      world_x = world_xyz[1],
      world_y = world_xyz[2],
      world_z = world_xyz[3],
      grid_x = grid_xyz[1],
      grid_y = grid_xyz[2],
      grid_z = grid_xyz[3]
    )
  }

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(empty)
  }
  dplyr::bind_rows(rows)
}

.build_plot_brain_surface_pick_lookup <- function(surfatlas,
                                                  stat_map,
                                                  views = c("lateral", "medial"),
                                                  hemis = c("left", "right"),
                                                  surface = "inflated") {
  empty <- .surface_pick_lookup_from_polygons(
    poly = NULL,
    panel_ctx = list(),
    stat_map = stat_map
  )

  if (!.has_surface_geometry(surfatlas)) {
    return(empty)
  }

  build <- .build_merged_polygon_data_memo(
    surfatlas = surfatlas,
    views = views,
    surface = surface
  )
  poly <- build$polygons
  if (is.null(poly) || nrow(poly) == 0) {
    return(empty)
  }

  poly <- poly[poly$hemi %in% hemis, , drop = FALSE]
  if (nrow(poly) == 0) {
    return(empty)
  }

  panel_ctx <- list()
  hemi_key <- c(left = "lh", right = "rh")
  for (h in hemis) {
    hk <- hemi_key[[h]]
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]
    if (is.null(atlas_hemi)) next

    geom <- atlas_hemi@geometry
    verts <- t(geom@mesh$vb[1:3, , drop = FALSE])
    parcels <- as.integer(atlas_hemi@data)
    if (length(parcels) != nrow(verts)) {
      parcels <- rep(NA_integer_, nrow(verts))
    }

    for (v in views) {
      panel <- paste0(tools::toTitleCase(h), " ", tools::toTitleCase(v))
      proj <- .project_view(verts = verts, view = v, hemi = h)
      panel_ctx[[panel]] <- list(
        xy = proj$xy,
        verts = verts,
        parcels = parcels,
        geometry = geom
      )
    }
  }

  .surface_pick_lookup_from_polygons(
    poly = poly,
    panel_ctx = panel_ctx,
    stat_map = stat_map
  )
}

.clusters_for_grid_centers <- function(cluster_voxels,
                                       centers,
                                       radius = 0,
                                       fallback_nearest = TRUE) {
  if (length(cluster_voxels) == 0) return(character(0))

  centers <- as.matrix(centers)
  if (is.null(dim(centers)) || ncol(centers) != 3 || nrow(centers) == 0) {
    return(character(0))
  }

  rad <- suppressWarnings(as.numeric(radius))
  if (!is.finite(rad) || rad < 0) rad <- 0
  rad2 <- rad^2

  ids <- names(cluster_voxels)
  if (is.null(ids)) ids <- paste0("K", seq_along(cluster_voxels))
  score <- rep(Inf, length(cluster_voxels))
  names(score) <- ids

  for (i in seq_along(cluster_voxels)) {
    vox <- as.matrix(cluster_voxels[[i]])
    if (is.null(vox) || nrow(vox) == 0 || ncol(vox) != 3) next

    dmin <- Inf
    for (j in seq_len(nrow(centers))) {
      ctr <- matrix(centers[j, ], nrow = nrow(vox), ncol = 3, byrow = TRUE)
      d2 <- rowSums((vox - ctr)^2)
      dmin <- min(dmin, min(d2))
    }
    score[i] <- dmin
  }

  inside <- names(score)[is.finite(score) & score <= rad2]
  if (length(inside) > 0) {
    return(inside[order(score[inside], decreasing = FALSE)])
  }

  if (isTRUE(fallback_nearest)) {
    finite <- which(is.finite(score))
    if (length(finite) == 0) return(character(0))
    best <- finite[which.min(score[finite])]
    return(names(score)[best])
  }

  character(0)
}

.parcel_values_from_clusters <- function(cluster_parcels,
                                         atlas_ids,
                                         selected_cluster_ids = NULL,
                                         mode = c("dominant",
                                                  "positive_only",
                                                  "negative_only")) {
  mode <- match.arg(mode)
  vals <- rep(NA_real_, length(atlas_ids))
  names(vals) <- as.character(atlas_ids)

  if (nrow(cluster_parcels) == 0) {
    return(vals)
  }

  cp <- cluster_parcels
  if (!is.null(selected_cluster_ids) && length(selected_cluster_ids) > 0) {
    cp <- cp[cp$cluster_id %in% selected_cluster_ids, , drop = FALSE]
  }
  if (nrow(cp) == 0) {
    return(vals)
  }

  parcel_groups <- split(cp, cp$parcel_id)
  out_vals <- vapply(parcel_groups, function(df) {
    if (mode == "dominant") {
      peak <- df$peak_stat[which.max(abs(df$peak_stat))]
      as.numeric(peak)
    } else if (mode == "positive_only") {
      x <- df$max_pos
      x <- x[is.finite(x)]
      if (length(x) == 0) NA_real_ else max(x)
    } else {
      x <- df$min_neg
      x <- x[is.finite(x)]
      if (length(x) == 0) NA_real_ else min(x)
    }
  }, numeric(1))

  common <- intersect(names(out_vals), names(vals))
  vals[common] <- out_vals[common]
  vals
}
