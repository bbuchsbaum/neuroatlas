# Global variables to avoid R CMD check NOTEs
utils::globalVariables(c("face_id", "vertex_order", "parcel_id", "panel",
                         "view", "tooltip", "data_id", "fill_color",
                         "fill_value", "xend", "yend", "poly_id", "edge_type",
                         "path_id", "v1", "v2", "alpha", "shade",
                         "overlay_color",
                         "xmin", "xmax", "ymin", "ymax",
                         "rotate", "height", "width"))

#' Compute face normals for a triangle mesh
#'
#' @param verts N x 3 matrix of vertex coordinates (x, y, z).
#' @param faces F x 3 integer matrix of vertex indices per face.
#' @return F x 3 matrix of unit face normals.
#' @keywords internal
#' @noRd
.face_normals <- function(verts, faces) {
  v1 <- verts[faces[, 1], , drop = FALSE]
  v2 <- verts[faces[, 2], , drop = FALSE]
  v3 <- verts[faces[, 3], , drop = FALSE]

  e1 <- v2 - v1
  e2 <- v3 - v1

  # Cross product
  nx <- e1[, 2] * e2[, 3] - e1[, 3] * e2[, 2]
  ny <- e1[, 3] * e2[, 1] - e1[, 1] * e2[, 3]
  nz <- e1[, 1] * e2[, 2] - e1[, 2] * e2[, 1]

  len <- sqrt(nx^2 + ny^2 + nz^2)
  len[len == 0] <- 1

  cbind(nx / len, ny / len, nz / len)
}

#' Assign parcel IDs to faces by majority vote
#'
#' @param parcel_ids Integer vector of per-vertex parcel IDs (length = number
#'   of vertices).
#' @param faces F x 3 integer matrix of vertex indices per face.
#' @return Integer vector of length F with parcel ID per face.
#' @keywords internal
#' @noRd
.face_parcel_ids <- function(parcel_ids, faces) {
  p1 <- parcel_ids[faces[, 1]]
  p2 <- parcel_ids[faces[, 2]]
  p3 <- parcel_ids[faces[, 3]]

  # Majority vote (fast vectorised): if any two agree, use that value
  agree12 <- p1 == p2
  agree13 <- p1 == p3
  result <- ifelse(agree12 | agree13, p1, p2)
  # When none agree, p2 == p3 case is caught by choosing p2 above (which

  # equals p3 when agree23 is TRUE), and truly all-different falls back to p1
  # via the first branch. This is acceptable for atlas parcels.
  result
}

#' Compute boundary edges from visible faces
#'
#' Identifies edges that lie on the boundary between different parcels or at
#' the silhouette of the visible mesh. Optionally flags between-network
#' boundaries when a parcel->network map is supplied.
#'
#' @param vis_faces F_vis x 3 integer matrix of vertex indices for visible faces.
#' @param vis_parcel Integer vector of parcel IDs per visible face.
#' @param proj_xy N x 2 matrix of 2D projected vertex coordinates.
#' @param id_to_network Optional named character vector mapping parcel IDs to
#'   network labels.
#' @return A tibble with columns \code{x, y, xend, yend, edge_type}, or
#'   \code{NULL} if no boundary edges are found.
#' @keywords internal
#' @noRd
.compute_boundary_edges <- function(vis_faces, vis_parcel, proj_xy,
                                    id_to_network = NULL) {
  n_vis <- nrow(vis_faces)
  if (n_vis == 0) return(NULL)

  # Build all half-edges (3 per face)
  v_a <- c(vis_faces[, 1], vis_faces[, 2], vis_faces[, 3])
  v_b <- c(vis_faces[, 2], vis_faces[, 3], vis_faces[, 1])
  e_parcel <- rep(vis_parcel, 3)

  # Normalize: smaller vertex index first
  e_min <- pmin(v_a, v_b)
  e_max <- pmax(v_a, v_b)

  # Unique integer key per undirected edge
  max_v <- max(e_max)
  e_key <- as.numeric(e_min) * (max_v + 1) + as.numeric(e_max)

  # Sort by key to find paired (shared) edges

  ord <- order(e_key)
  sk <- e_key[ord]
  sp <- e_parcel[ord]
  sm <- e_min[ord]
  sx <- e_max[ord]

  n <- length(sk)

  # Consecutive entries with the same key share an edge
  same <- sk[-n] == sk[-1]
  diff_p <- sp[-n] != sp[-1]

  # Inter-parcel boundary: shared edge with different parcels on each side
  boundary_pair_idx <- which(same & diff_p)

  # Mesh / view boundary: edges appearing only once (adjacent face culled)
  is_paired <- logical(n)
  paired_first <- which(same)
  is_paired[paired_first] <- TRUE
  is_paired[paired_first + 1L] <- TRUE
  mesh_boundary_idx <- which(!is_paired)

  if (length(boundary_pair_idx) == 0 && length(mesh_boundary_idx) == 0) {
    return(NULL)
  }

  out <- list()

  if (length(boundary_pair_idx) > 0) {
    edge_type <- rep("parcel", length(boundary_pair_idx))

    if (!is.null(id_to_network)) {
      nets <- id_to_network[as.character(sp)]
      n1 <- nets[boundary_pair_idx]
      n2 <- nets[boundary_pair_idx + 1L]
      is_net <- !is.na(n1) & !is.na(n2) & n1 != n2
      edge_type[is_net] <- "network"
    }

    b_min <- sm[boundary_pair_idx]
    b_max <- sx[boundary_pair_idx]
    out[[length(out) + 1L]] <- tibble::tibble(
      v1 = b_min,
      v2 = b_max,
      x = proj_xy[b_min, 1],
      y = proj_xy[b_min, 2],
      xend = proj_xy[b_max, 1],
      yend = proj_xy[b_max, 2],
      edge_type = edge_type
    )
  }

  if (length(mesh_boundary_idx) > 0) {
    b_min <- sm[mesh_boundary_idx]
    b_max <- sx[mesh_boundary_idx]
    out[[length(out) + 1L]] <- tibble::tibble(
      v1 = b_min,
      v2 = b_max,
      x = proj_xy[b_min, 1],
      y = proj_xy[b_min, 2],
      xend = proj_xy[b_max, 1],
      yend = proj_xy[b_max, 2],
      edge_type = "silhouette"
    )
  }

  dplyr::bind_rows(out)
}

#' Chain boundary edges into ordered vertex paths
#'
#' Converts undirected boundary edges into maximal paths between junctions
#' (vertices with degree != 2), plus closed cycles when no such junctions exist.
#' Intended for smoother rendering than drawing many disjoint segments.
#'
#' @param edges Tibble from [.compute_boundary_edges()] with columns
#'   \code{panel}, \code{edge_type}, \code{v1}, \code{v2}, \code{x}, \code{y},
#'   \code{xend}, \code{yend}.
#' @return Tibble with columns \code{x, y, path_id, vertex_order, edge_type, panel},
#'   or \code{NULL} if no paths can be formed.
#' @keywords internal
#' @noRd
.boundary_edges_to_paths <- function(edges) {
  needed <- c("panel", "edge_type", "v1", "v2", "x", "y", "xend", "yend")
  if (is.null(edges) || nrow(edges) == 0) return(NULL)
  if (!all(needed %in% names(edges))) return(NULL)

  edges <- edges[!is.na(edges$v1) & !is.na(edges$v2), , drop = FALSE]
  if (nrow(edges) == 0) return(NULL)

  split_groups <- split(
    seq_len(nrow(edges)),
    interaction(edges$panel, edges$edge_type, drop = TRUE)
  )

  out <- list()
  path_id_global <- 0L

  for (idx in split_groups) {
    sub <- edges[idx, , drop = FALSE]
    if (nrow(sub) == 0) next

    # Build vertex -> coordinate map (panel-local)
    v_all <- c(sub$v1, sub$v2)
    x_all <- c(sub$x, sub$xend)
    y_all <- c(sub$y, sub$yend)
    v_key <- as.character(v_all)

    first_idx <- !duplicated(v_key)
    vx <- stats::setNames(x_all[first_idx], v_key[first_idx])
    vy <- stats::setNames(y_all[first_idx], v_key[first_idx])

    uniq_v <- sort(unique(v_all))
    n_v <- length(uniq_v)
    v_map <- stats::setNames(seq_len(n_v), as.character(uniq_v))

    a <- unname(v_map[as.character(sub$v1)])
    b <- unname(v_map[as.character(sub$v2)])
    n_e <- length(a)

    # Degree and incidence lists (on compact vertex indices)
    deg <- tabulate(c(a, b), nbins = n_v)
    inc <- vector("list", n_v)
    for (i in seq_len(n_e)) {
      inc[[a[i]]] <- c(inc[[a[i]]], i)
      inc[[b[i]]] <- c(inc[[b[i]]], i)
    }

    visited <- rep(FALSE, n_e)

    other_vertex <- function(edge_i, v_i) {
      if (a[edge_i] == v_i) b[edge_i] else a[edge_i]
    }

    walk_trail <- function(start_v, start_edge) {
      v_seq <- start_v
      cur_v <- start_v
      cur_e <- start_edge

      repeat {
        visited[cur_e] <<- TRUE
        nxt_v <- other_vertex(cur_e, cur_v)
        v_seq <- c(v_seq, nxt_v)

        if (deg[nxt_v] != 2) break

        cand <- inc[[nxt_v]]
        cand <- cand[!visited[cand]]
        if (length(cand) == 0) break

        cur_v <- nxt_v
        cur_e <- cand[1]
      }

      v_seq
    }

    walk_cycle <- function(start_edge) {
      start_v <- a[start_edge]
      v_seq <- start_v
      cur_v <- start_v
      cur_e <- start_edge

      repeat {
        visited[cur_e] <<- TRUE
        nxt_v <- other_vertex(cur_e, cur_v)
        v_seq <- c(v_seq, nxt_v)

        if (nxt_v == start_v) break

        cand <- inc[[nxt_v]]
        cand <- cand[!visited[cand]]
        if (length(cand) == 0) break

        cur_v <- nxt_v
        cur_e <- cand[1]
      }

      v_seq
    }

    # Start trails from junction/end vertices (deg != 2)
    special <- which(deg != 2 & deg > 0)
    if (length(special) > 0) {
      for (sv in special) {
        for (e in inc[[sv]]) {
          if (visited[e]) next
          v_seq <- walk_trail(sv, e)

          path_id_global <- path_id_global + 1L
          orig_v <- uniq_v[v_seq]
          v_chr <- as.character(orig_v)

          out[[length(out) + 1L]] <- tibble::tibble(
            x = unname(vx[v_chr]),
            y = unname(vy[v_chr]),
            path_id = path_id_global,
            vertex_order = seq_along(orig_v),
            edge_type = sub$edge_type[[1]],
            panel = sub$panel[[1]]
          )
        }
      }
    }

    # Remaining edges form cycles (all deg == 2)
    while (any(!visited)) {
      start_edge <- which(!visited)[1]
      v_seq <- walk_cycle(start_edge)

      path_id_global <- path_id_global + 1L
      orig_v <- uniq_v[v_seq]
      v_chr <- as.character(orig_v)

      out[[length(out) + 1L]] <- tibble::tibble(
        x = unname(vx[v_chr]),
        y = unname(vy[v_chr]),
        path_id = path_id_global,
        vertex_order = seq_along(orig_v),
        edge_type = sub$edge_type[[1]],
        panel = sub$panel[[1]]
      )
    }
  }

  if (length(out) == 0) return(NULL)
  dplyr::bind_rows(out)
}

#' Chaikin corner-cutting for 2D polylines
#'
#' @param x Numeric x coordinates.
#' @param y Numeric y coordinates.
#' @param n_iter Non-negative integer number of smoothing iterations.
#' @return A list with smoothed numeric vectors \code{x} and \code{y}.
#' @keywords internal
#' @noRd
.chaikin_smooth_xy <- function(x, y, n_iter = 1L) {
  n_iter <- as.integer(n_iter)
  if (n_iter <= 0L) return(list(x = x, y = y))

  pts <- cbind(as.numeric(x), as.numeric(y))
  n <- nrow(pts)
  if (n < 3L) return(list(x = pts[, 1], y = pts[, 2]))

  tol <- 1e-8
  is_closed <- n >= 4L &&
    abs(pts[1, 1] - pts[n, 1]) < tol &&
    abs(pts[1, 2] - pts[n, 2]) < tol

  if (is_closed) {
    pts <- pts[-n, , drop = FALSE]
  }

  for (iter in seq_len(n_iter)) {
    n0 <- nrow(pts)
    if (n0 < 3L) break

    if (is_closed) {
      out <- matrix(0, nrow = 2L * n0, ncol = 2L)
      for (i in seq_len(n0)) {
        j <- if (i == n0) 1L else i + 1L
        p <- pts[i, ]
        q <- pts[j, ]
        out[2L * i - 1L, ] <- 0.75 * p + 0.25 * q
        out[2L * i, ] <- 0.25 * p + 0.75 * q
      }
    } else {
      out <- matrix(0, nrow = 2L * n0, ncol = 2L)
      out[1, ] <- pts[1, ]
      k <- 2L
      for (i in seq_len(n0 - 1L)) {
        p <- pts[i, ]
        q <- pts[i + 1L, ]
        out[k, ] <- 0.75 * p + 0.25 * q
        out[k + 1L, ] <- 0.25 * p + 0.75 * q
        k <- k + 2L
      }
      out[k, ] <- pts[n0, ]
    }

    pts <- out
  }

  if (is_closed) {
    pts <- rbind(pts, pts[1, , drop = FALSE])
  }

  list(x = pts[, 1], y = pts[, 2])
}

#' Smooth boundary paths for cleaner rendered parcel edges
#'
#' @param paths Tibble produced by [.boundary_edges_to_paths()].
#' @param n_iter Non-negative integer number of Chaikin iterations.
#' @return Smoothed path tibble.
#' @keywords internal
#' @noRd
.smooth_boundary_paths <- function(paths, n_iter = 1L) {
  if (is.null(paths) || nrow(paths) == 0) return(paths)

  n_iter <- as.integer(n_iter)
  if (is.na(n_iter) || n_iter <= 0L) return(paths)

  needed <- c("x", "y", "path_id", "vertex_order", "edge_type", "panel")
  if (!all(needed %in% names(paths))) return(paths)

  split_paths <- split(paths, paths$path_id)
  out <- vector("list", length(split_paths))
  i <- 1L

  for (sub in split_paths) {
    sub <- sub[order(sub$vertex_order), , drop = FALSE]
    if (nrow(sub) < 3L) {
      sub$vertex_order <- seq_len(nrow(sub))
      out[[i]] <- sub
      i <- i + 1L
      next
    }

    sm <- .chaikin_smooth_xy(sub$x, sub$y, n_iter = n_iter)

    out[[i]] <- tibble::tibble(
      x = sm$x,
      y = sm$y,
      path_id = sub$path_id[[1]],
      vertex_order = seq_along(sm$x),
      edge_type = sub$edge_type[[1]],
      panel = sub$panel[[1]]
    )
    i <- i + 1L
  }

  dplyr::bind_rows(out)
}

#' Chain boundary half-edges into ordered polygon loops per parcel
#'
#' For each parcel in a single view, finds the directed boundary half-edges
#' (edges where the adjacent face belongs to a different parcel or was culled)
#' and chains them into ordered vertex loops suitable for
#' \code{ggplot2::geom_polygon}.
#'
#' @param vis_faces F_vis x 3 integer matrix of vertex indices for visible faces.
#' @param vis_parcel Integer vector of parcel IDs per visible face.
#' @param proj_xy N x 2 matrix of 2D projected vertex coordinates.
#' @param id_to_label Named character vector mapping parcel IDs to labels.
#' @return A tibble with columns: x, y, poly_id, parcel_id, label.
#' @keywords internal
#' @noRd
.merge_parcel_polygons <- function(vis_faces, vis_parcel, proj_xy,
                                   id_to_label) {
  n_vis <- nrow(vis_faces)
  if (n_vis == 0) return(NULL)

  # Build undirected boundary edges per parcel.
  #
  # Note: The fsaverage meshes shipped by neurosurf do not guarantee that
  # triangle vertex winding is globally consistent. Boundary detection must
  # therefore be orientation-invariant. We count undirected edges within each
  # parcel: edges that occur exactly once are parcel boundary edges.
  e_a <- c(vis_faces[, 1], vis_faces[, 2], vis_faces[, 3])
  e_b <- c(vis_faces[, 2], vis_faces[, 3], vis_faces[, 1])
  e_parcel <- rep(vis_parcel, 3)

  e_min <- pmin(e_a, e_b)
  e_max <- pmax(e_a, e_b)

  max_v <- max(e_max)
  stride <- max_v + 1
  e_key <- as.numeric(e_parcel) * stride * stride +
           as.numeric(e_min) * stride +
           as.numeric(e_max)

  ord <- order(e_key)
  sk <- e_key[ord]
  sp <- e_parcel[ord]
  su <- e_min[ord]
  sv <- e_max[ord]

  n <- length(sk)
  if (n == 0) return(NULL)

  # Run-length encode identical (parcel, edge) keys
  is_break <- c(TRUE, diff(sk) != 0)
  run_id <- cumsum(is_break)
  run_len <- tabulate(run_id)
  boundary_run <- run_len[run_id] == 1L

  b_parcel <- sp[boundary_run]
  b_u <- su[boundary_run]
  b_v <- sv[boundary_run]

  if (length(b_u) == 0) return(NULL)

  unique_parcels <- sort(unique(b_parcel))
  poly_list <- vector("list", length(unique_parcels) * 2L)
  poly_count <- 0L

  for (pid in unique_parcels) {
    mask <- b_parcel == pid
    u <- b_u[mask]
    v <- b_v[mask]
    n_edges <- length(u)
    if (n_edges < 3) next

    # Build adjacency list keyed by vertex, storing (neighbor, edge_id)
    edge_id <- seq_len(n_edges)
    from <- c(u, v)
    to <- c(v, u)
    eid <- rep(edge_id, 2)

    adj <- split(data.frame(to = to, eid = eid), from)
    edge_used <- rep(FALSE, n_edges)

    # Walk unused edges to form boundary loops.
    while (any(!edge_used)) {
      start_edge <- which(!edge_used)[1]
      start <- u[start_edge]
      curr <- v[start_edge]
      prev <- start
      edge_used[start_edge] <- TRUE

      loop <- c(start, curr)

      repeat {
        if (curr == start) break

        nbrs <- adj[[as.character(curr)]]
        if (is.null(nbrs) || nrow(nbrs) == 0) break

        cand <- which(!edge_used[nbrs$eid])
        if (length(cand) == 0) break

        # Prefer not to immediately backtrack when possible
        if (!is.na(prev)) {
          cand2 <- cand[nbrs$to[cand] != prev]
          if (length(cand2) > 0) cand <- cand2
        }

        # If multiple choices remain (non-manifold boundary), take the
        # straightest continuation in the projected 2D plane.
        chosen <- cand[1]
        if (length(cand) > 1 && !is.na(prev)) {
          in_vec <- proj_xy[curr, , drop = FALSE] - proj_xy[prev, , drop = FALSE]
          in_norm <- sqrt(sum(in_vec^2))
          if (is.finite(in_norm) && in_norm > 0) {
            scores <- vapply(cand, function(k) {
              nv <- nbrs$to[k]
              out_vec <- proj_xy[nv, , drop = FALSE] - proj_xy[curr, , drop = FALSE]
              out_norm <- sqrt(sum(out_vec^2))
              if (!is.finite(out_norm) || out_norm == 0) return(-Inf)
              sum(in_vec * out_vec) / (in_norm * out_norm)
            }, numeric(1))
            chosen <- cand[which.max(scores)]
          }
        }

        next_v <- nbrs$to[chosen]
        edge_used[nbrs$eid[chosen]] <- TRUE
        prev <- curr
        curr <- next_v
        loop <- c(loop, curr)
      }

      # Keep only closed loops with at least 3 vertices
      if (length(loop) >= 4 && loop[length(loop)] == loop[1]) {
        loop <- loop[-length(loop)]
      } else {
        next
      }

      if (length(loop) >= 3) {
        poly_count <- poly_count + 1L
        lbl <- id_to_label[as.character(pid)]
        if (is.na(lbl)) lbl <- ""

        poly_list[[poly_count]] <- tibble::tibble(
          x = proj_xy[loop, 1],
          y = proj_xy[loop, 2],
          poly_id = poly_count,
          parcel_id = pid,
          label = lbl
        )
      }
    }
  }

  if (poly_count == 0L) return(NULL)
  dplyr::bind_rows(poly_list[seq_len(poly_count)])
}

#' Build merged polygon data for brain surface rendering
#'
#' Produces one polygon per parcel per view (instead of one per triangle),
#' dramatically reducing the number of SVG elements for interactive rendering.
#'
#' @param surfatlas A surfatlas object.
#' @param views Character vector of views to render.
#' @param surface Character: surface type.
#' @return A list with \code{polygons} and \code{boundaries} tibbles.
#' @keywords internal
#' @noRd
.build_merged_polygon_data <- function(surfatlas, views, surface,
                                       projection_smooth = 0L) {
  hemis <- c("left", "right")
  hemi_keys <- c(left = "lh", right = "rh")

  all_labels <- surfatlas$labels
  all_ids <- surfatlas$ids
  id_to_label <- stats::setNames(all_labels, as.character(all_ids))
  id_to_network <- NULL
  if (!is.null(surfatlas$network) &&
      length(surfatlas$network) == length(surfatlas$ids)) {
    id_to_network <- stats::setNames(as.character(surfatlas$network),
                                     as.character(surfatlas$ids))
  }

  polygon_list <- list()
  boundary_list <- list()
  global_offset <- 0L

  for (h in hemis) {
    hk <- hemi_keys[[h]]
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]

    geom <- atlas_hemi@geometry
    verts <- t(geom@mesh$vb[1:3, ])
    faces <- t(geom@mesh$it)
    parcel_vec <- as.integer(atlas_hemi@data)
    neighbors <- NULL
    if (projection_smooth > 0L) {
      neighbors <- .mesh_vertex_neighbors(faces, nrow(verts))
    }

    fnormals <- .face_normals(verts, faces)
    fparcel <- .face_parcel_ids(parcel_vec, faces)

    for (v in views) {
      proj <- .project_view(verts, v, h)
      if (projection_smooth > 0L) {
        proj$xy <- .smooth_projected_xy(
          proj$xy,
          neighbors = neighbors,
          n_iter = projection_smooth
        )
      }
      vdir <- proj$view_dir

      dots <- fnormals[, 1] * vdir[1] +
              fnormals[, 2] * vdir[2] +
              fnormals[, 3] * vdir[3]
      visible <- which(dots > 0 & fparcel != 0)
      if (length(visible) == 0) next

      vis_faces  <- faces[visible, , drop = FALSE]
      vis_parcel <- fparcel[visible]
      panel_label <- paste0(tools::toTitleCase(h), " ",
                            tools::toTitleCase(v))

      # Merged parcel polygons
      merged <- .merge_parcel_polygons(vis_faces, vis_parcel, proj$xy,
                                       id_to_label)
      if (!is.null(merged)) {
        merged$poly_id <- merged$poly_id + global_offset
        global_offset <- max(merged$poly_id)
        merged$hemi  <- h
        merged$view  <- v
        merged$panel <- panel_label
        polygon_list[[length(polygon_list) + 1L]] <- merged
      }

      # Boundary edges for the border line layer
      bedge <- .compute_boundary_edges(vis_faces, vis_parcel, proj$xy,
                                       id_to_network = id_to_network)
      if (!is.null(bedge)) {
        bedge$panel <- panel_label
        boundary_list[[length(boundary_list) + 1L]] <- bedge
      }
    }
  }

  list(
    polygons   = dplyr::bind_rows(polygon_list),
    boundaries = dplyr::bind_rows(boundary_list)
  )
}

#' Memoised version of .build_merged_polygon_data
#' @keywords internal
#' @noRd
.build_merged_polygon_data_memo <- memoise::memoise(.build_merged_polygon_data)

#' Project 3D vertices to 2D for a given view
#'
#' Returns projected 2D coordinates and the view direction vector used for
#' backface culling.
#'
#' @param verts N x 3 matrix of vertex coordinates.
#' @param view Character: one of "lateral", "medial", "dorsal", "ventral".
#' @param hemi Character: "left" or "right".
#' @return A list with elements \code{xy} (N x 2 matrix), \code{view_dir}
#'   (length-3 numeric vector).
#' @keywords internal
#' @noRd
.project_view <- function(verts, view, hemi) {
  # Define projection based on view + hemisphere
  if (view == "lateral" && hemi == "left") {
    view_dir <- c(-1, 0, 0)
    xy <- cbind(verts[, 2], verts[, 3])          # (y, z)
  } else if (view == "medial" && hemi == "left") {
    view_dir <- c(1, 0, 0)
    xy <- cbind(-verts[, 2], verts[, 3])          # (-y, z)
  } else if (view == "lateral" && hemi == "right") {
    view_dir <- c(1, 0, 0)
    xy <- cbind(-verts[, 2], verts[, 3])          # (-y, z)
  } else if (view == "medial" && hemi == "right") {
    view_dir <- c(-1, 0, 0)
    xy <- cbind(verts[, 2], verts[, 3])           # (y, z)
  } else if (view == "dorsal") {
    view_dir <- c(0, 0, 1)
    xy <- if (hemi == "left") {
      cbind(verts[, 1], verts[, 2])               # (x, y)
    } else {
      cbind(-verts[, 1], verts[, 2])              # (-x, y)
    }
  } else if (view == "ventral") {
    view_dir <- c(0, 0, -1)
    xy <- if (hemi == "left") {
      cbind(verts[, 1], -verts[, 2])              # (x, -y)
    } else {
      cbind(-verts[, 1], -verts[, 2])             # (-x, -y)
    }
  } else {
    stop("Unknown view: ", view)
  }

  list(xy = xy, view_dir = view_dir)
}

#' Build per-vertex mesh neighbour lists from triangular faces
#'
#' @param faces Integer matrix (\code{F x 3}) of vertex indices.
#' @param n_vertices Total number of mesh vertices.
#' @return A list of integer neighbour vectors (length \code{n_vertices}).
#' @keywords internal
#' @noRd
.mesh_vertex_neighbors <- function(faces, n_vertices) {
  if (is.null(faces) || nrow(faces) == 0 || n_vertices <= 0) {
    return(vector("list", max(0L, as.integer(n_vertices))))
  }

  edges <- rbind(
    faces[, c(1, 2), drop = FALSE],
    faces[, c(2, 3), drop = FALSE],
    faces[, c(3, 1), drop = FALSE]
  )
  edges <- rbind(edges, edges[, c(2, 1), drop = FALSE])
  edges <- edges[edges[, 1] != edges[, 2], , drop = FALSE]
  edges <- edges[order(edges[, 1], edges[, 2]), , drop = FALSE]

  neigh <- vector("list", as.integer(n_vertices))
  if (nrow(edges) == 0) return(neigh)

  split_nb <- split(edges[, 2], edges[, 1])
  idx <- as.integer(names(split_nb))
  for (k in seq_along(split_nb)) {
    i <- idx[[k]]
    if (!is.na(i) && i >= 1 && i <= n_vertices) {
      neigh[[i]] <- unique(as.integer(split_nb[[k]]))
    }
  }

  neigh
}

#' Smooth projected 2D coordinates over mesh adjacency
#'
#' Applies iterative Laplacian-like smoothing in projected space while
#' preserving shared parcel boundaries (all parcels share the same smoothed
#' vertex coordinates).
#'
#' @param xy Numeric matrix (\code{N x 2}) of projected coordinates.
#' @param neighbors List of integer neighbour indices from
#'   [.mesh_vertex_neighbors()].
#' @param n_iter Non-negative integer smoothing iterations.
#' @param lambda Numeric blend weight in \code{(0, 1)}. Larger values smooth
#'   more aggressively.
#' @return Smoothed \code{xy} matrix.
#' @keywords internal
#' @noRd
.smooth_projected_xy <- function(xy, neighbors, n_iter = 1L, lambda = 0.35) {
  n_iter <- as.integer(n_iter)
  if (is.na(n_iter) || n_iter <= 0L) return(xy)
  if (!is.matrix(xy) || ncol(xy) != 2) return(xy)
  if (!is.list(neighbors) || length(neighbors) != nrow(xy)) return(xy)
  if (!is.numeric(lambda) || length(lambda) != 1 || is.na(lambda) ||
      lambda <= 0 || lambda >= 1) return(xy)

  out <- xy
  n <- nrow(out)

  for (iter in seq_len(n_iter)) {
    x_new <- out[, 1]
    y_new <- out[, 2]

    for (i in seq_len(n)) {
      nb <- neighbors[[i]]
      if (length(nb) == 0L) next
      mx <- mean(out[nb, 1])
      my <- mean(out[nb, 2])
      x_new[i] <- (1 - lambda) * out[i, 1] + lambda * mx
      y_new[i] <- (1 - lambda) * out[i, 2] + lambda * my
    }

    out[, 1] <- x_new
    out[, 2] <- y_new
  }

  out
}

#' Project Surface Vertices to a Canonical 2D View
#'
#' Public wrapper for the core surface-view projection used by
#' \code{\link{plot_brain}()}.
#'
#' @param verts Numeric matrix (\eqn{N \times 3}) of vertex coordinates.
#' @param view Character scalar: one of \code{"lateral"}, \code{"medial"},
#'   \code{"dorsal"}, \code{"ventral"}.
#' @param hemi Character scalar: \code{"left"} or \code{"right"}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{xy}}{Numeric matrix (\eqn{N \times 2}) of projected coordinates.}
#'   \item{\code{view_dir}}{Numeric length-3 view direction vector used for
#'     backface culling.}
#' }
#' @export
project_surface_view <- function(verts,
                                 view = c("lateral", "medial",
                                          "dorsal", "ventral"),
                                 hemi = c("left", "right")) {
  view <- match.arg(view)
  hemi <- match.arg(hemi)

  if (!is.matrix(verts) || ncol(verts) != 3) {
    stop("'verts' must be a numeric matrix with 3 columns.", call. = FALSE)
  }

  .project_view(verts = verts, view = view, hemi = hemi)
}

#' Encode stable interactive IDs for plot_brain polygons
#'
#' @param panel Character vector of panel labels.
#' @param parcel_id Integer/numeric parcel ids.
#' @param shape_id Integer/numeric shape ids (poly_id or face_id).
#' @return Character vector of encoded ids.
#' @keywords internal
#' @noRd
.encode_plot_brain_data_id <- function(panel, parcel_id, shape_id) {
  paste(
    as.character(panel),
    as.integer(parcel_id),
    as.integer(shape_id),
    sep = "::"
  )
}

#' Build polygon data for brain surface rendering
#'
#' Generates a tibble of 2D projected polygon vertices for all requested
#' hemispheres and views, suitable for \code{ggplot2::geom_polygon}.
#'
#' @param surfatlas A surfatlas object (e.g. from \code{schaefer_surf()}).
#' @param views Character vector of views to render.
#' @param surface Character: surface type ("inflated", "pial", "white").
#' @return A list with two elements: \code{polygons} (tibble with columns x, y,
#'   face_id, vertex_order, parcel_id, hemi, view, panel, label) and
#'   \code{boundaries} (tibble with columns x, y, xend, yend, panel).
#' @keywords internal
#' @noRd
.build_brain_polygon_data <- function(surfatlas, views, surface,
                                      projection_smooth = 0L) {
  hemis <- c("left", "right")
  hemi_keys <- c(left = "lh", right = "rh")

  all_labels <- surfatlas$labels
  all_ids <- surfatlas$ids
  # Build label lookup: parcel_id -> label
  id_to_label <- stats::setNames(all_labels, as.character(all_ids))
  id_to_network <- NULL
  if (!is.null(surfatlas$network) &&
      length(surfatlas$network) == length(surfatlas$ids)) {
    id_to_network <- stats::setNames(as.character(surfatlas$network),
                                     as.character(surfatlas$ids))
  }

  polygon_list <- list()
  boundary_list <- list()
  global_face_id <- 0L

  for (h in hemis) {
    hk <- hemi_keys[[h]]
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]

    # Extract geometry from the LabeledNeuroSurface
    geom <- atlas_hemi@geometry
    verts <- t(geom@mesh$vb[1:3, ])   # N x 3
    faces <- t(geom@mesh$it)           # F x 3
    neighbors <- NULL
    if (projection_smooth > 0L) {
      neighbors <- .mesh_vertex_neighbors(faces, nrow(verts))
    }

    # Per-vertex parcel IDs
    parcel_vec <- as.integer(atlas_hemi@data)

    # Compute face normals and per-face parcel IDs
    fnormals <- .face_normals(verts, faces)
    fparcel <- .face_parcel_ids(parcel_vec, faces)

    for (v in views) {
      proj <- .project_view(verts, v, h)
      if (projection_smooth > 0L) {
        proj$xy <- .smooth_projected_xy(
          proj$xy,
          neighbors = neighbors,
          n_iter = projection_smooth
        )
      }
      vdir <- proj$view_dir

      # Backface culling: keep faces whose normal dots positively with view dir
      dots <- fnormals[, 1] * vdir[1] + fnormals[, 2] * vdir[2] + fnormals[, 3] * vdir[3]
      visible <- which(dots > 0 & fparcel != 0)

      if (length(visible) == 0) next

      n_vis <- length(visible)
      fids <- global_face_id + seq_len(n_vis)
      global_face_id <- global_face_id + n_vis

      # Build 3-row-per-face tibble
      vis_faces <- faces[visible, , drop = FALSE]
      vis_parcel <- fparcel[visible]
      vis_shade <- dots[visible]

      x_coords <- c(proj$xy[vis_faces[, 1], 1],
                     proj$xy[vis_faces[, 2], 1],
                     proj$xy[vis_faces[, 3], 1])
      y_coords <- c(proj$xy[vis_faces[, 1], 2],
                     proj$xy[vis_faces[, 2], 2],
                     proj$xy[vis_faces[, 3], 2])

      panel_label <- paste0(tools::toTitleCase(h), " ", tools::toTitleCase(v))

      # Look up labels for these parcel IDs
      face_labels <- id_to_label[as.character(vis_parcel)]
      face_labels[is.na(face_labels)] <- ""

      chunk <- tibble::tibble(
        x = x_coords,
        y = y_coords,
        face_id = rep(fids, 3),
        vertex_order = rep(1:3, each = n_vis),
        parcel_id = rep(vis_parcel, 3),
        shade = rep(vis_shade, 3),
        hemi = h,
        view = v,
        panel = panel_label,
        label = rep(face_labels, 3)
      )
      polygon_list[[length(polygon_list) + 1L]] <- chunk

      # Compute parcel boundary edges for this view
      bedge <- .compute_boundary_edges(vis_faces, vis_parcel, proj$xy,
                                       id_to_network = id_to_network)
      if (!is.null(bedge)) {
        bedge$panel <- panel_label
        boundary_list[[length(boundary_list) + 1L]] <- bedge
      }
    }
  }

  list(
    polygons = dplyr::bind_rows(polygon_list),
    boundaries = dplyr::bind_rows(boundary_list)
  )
}

#' Memoised version of .build_brain_polygon_data
#' @keywords internal
#' @noRd
.build_brain_polygon_data_memo <- memoise::memoise(.build_brain_polygon_data)

#' Build Surface Polygon Data for Rendering
#'
#' Build 2D projected polygon/boundary data from a \code{surfatlas} for custom
#' rendering workflows. This exposes the mesh-projection data pipeline used
#' internally by \code{\link{plot_brain}()}.
#'
#' @param surfatlas A surface atlas object inheriting from class
#'   \code{"surfatlas"}.
#' @param views Character vector of views to include. Any combination of
#'   \code{"lateral"}, \code{"medial"}, \code{"dorsal"}, \code{"ventral"}.
#' @param surface Character scalar identifying the surface type label.
#' @param merged Logical. If \code{TRUE} (default), returns merged
#'   parcel-level polygons (faster, fewer shapes). If \code{FALSE}, returns
#'   per-triangle polygons.
#' @param projection_smooth Non-negative integer controlling Laplacian-like
#'   smoothing iterations applied to projected vertex coordinates before
#'   polygon and boundary construction. \code{0} (default) keeps native mesh
#'   coordinates.
#' @param use_cache Logical. If \code{TRUE} (default), use memoized builders.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{polygons}}{A tibble of projected polygon vertices.}
#'   \item{\code{boundaries}}{A tibble of projected boundary segments.}
#' }
#' @export
build_surface_polygon_data <- function(surfatlas,
                                       views = c("lateral", "medial"),
                                       surface = "inflated",
                                       merged = TRUE,
                                       projection_smooth = 0L,
                                       use_cache = TRUE) {
  if (!inherits(surfatlas, "surfatlas")) {
    stop("'surfatlas' must inherit from class 'surfatlas'.", call. = FALSE)
  }

  views <- match.arg(
    views,
    choices = c("lateral", "medial", "dorsal", "ventral"),
    several.ok = TRUE
  )

  if (!is.logical(merged) || length(merged) != 1 || is.na(merged)) {
    stop("'merged' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(use_cache) || length(use_cache) != 1 || is.na(use_cache)) {
    stop("'use_cache' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(projection_smooth) || length(projection_smooth) != 1 ||
      is.na(projection_smooth) || projection_smooth < 0 ||
      projection_smooth != as.integer(projection_smooth)) {
    stop("'projection_smooth' must be a non-negative integer scalar.",
         call. = FALSE)
  }
  projection_smooth <- as.integer(projection_smooth)

  if (isTRUE(merged)) {
    builder <- if (isTRUE(use_cache)) {
      .build_merged_polygon_data_memo
    } else {
      .build_merged_polygon_data
    }
  } else {
    builder <- if (isTRUE(use_cache)) {
      .build_brain_polygon_data_memo
    } else {
      .build_brain_polygon_data
    }
  }

  builder(
    surfatlas = surfatlas,
    views = views,
    surface = surface,
    projection_smooth = projection_smooth
  )
}


#' Build overlay face polygons from vertex-wise values
#'
#' @param surfatlas A surface atlas.
#' @param overlay A list with components \code{lh} and \code{rh} containing
#'   vertex-wise numeric vectors.
#' @param views Character vector of views.
#' @param hemis Character vector of hemispheres.
#' @param threshold Optional absolute threshold for overlay face values.
#' @return A list with \code{polygons} and \code{boundaries}.
#' @keywords internal
#' @noRd
.build_overlay_polygon_data <- function(surfatlas,
                                        overlay,
                                        views,
                                        hemis,
                                        projection_smooth = 0L,
                                        threshold = NULL) {
  hemi_keys <- c(left = "lh", right = "rh")
  poly_list <- list()
  boundary_list <- list()
  global_face_id <- 0L

  for (h in hemis) {
    hk <- hemi_keys[[h]]
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]
    geom <- atlas_hemi@geometry
    verts <- t(geom@mesh$vb[1:3, , drop = FALSE])
    faces <- t(geom@mesh$it)
    neighbors <- NULL
    if (projection_smooth > 0L) {
      neighbors <- .mesh_vertex_neighbors(faces, nrow(verts))
    }
    fnormals <- .face_normals(verts, faces)

    ov <- overlay[[hk]]
    if (is.null(ov)) next
    ov <- as.numeric(ov)
    if (length(ov) != nrow(verts)) next

    for (v in views) {
      proj <- .project_view(verts, v, h)
      if (projection_smooth > 0L) {
        proj$xy <- .smooth_projected_xy(
          proj$xy,
          neighbors = neighbors,
          n_iter = projection_smooth
        )
      }
      vdir <- proj$view_dir
      dots <- fnormals[, 1] * vdir[1] +
        fnormals[, 2] * vdir[2] +
        fnormals[, 3] * vdir[3]

      visible <- which(dots > 0)
      if (length(visible) == 0) next

      vis_faces <- faces[visible, , drop = FALSE]
      fvals <- rowMeans(
        cbind(
          ov[vis_faces[, 1]],
          ov[vis_faces[, 2]],
          ov[vis_faces[, 3]]
        ),
        na.rm = TRUE
      )

      finite <- is.finite(fvals)
      if (!is.null(threshold) && is.finite(threshold)) {
        finite <- finite & abs(fvals) >= threshold
      }

      panel_label <- paste0(tools::toTitleCase(h), " ", tools::toTitleCase(v))

      # Boundary between overlay-in and overlay-out regions
      face_bin <- integer(length(fvals))
      face_bin[finite] <- 1L
      if (any(face_bin == 1L)) {
        bedge <- .compute_boundary_edges(vis_faces, face_bin, proj$xy)
        if (!is.null(bedge)) {
          bedge <- bedge[bedge$edge_type == "parcel", , drop = FALSE]
          if (nrow(bedge) > 0) {
            bedge$panel <- panel_label
            boundary_list[[length(boundary_list) + 1L]] <- bedge
          }
        }
      }

      keep_faces <- which(finite)
      if (length(keep_faces) == 0) next

      vis_faces_keep <- vis_faces[keep_faces, , drop = FALSE]
      vals_keep <- fvals[keep_faces]
      n_keep <- nrow(vis_faces_keep)

      fids <- global_face_id + seq_len(n_keep)
      global_face_id <- global_face_id + n_keep

      chunk <- tibble::tibble(
        x = c(proj$xy[vis_faces_keep[, 1], 1],
              proj$xy[vis_faces_keep[, 2], 1],
              proj$xy[vis_faces_keep[, 3], 1]),
        y = c(proj$xy[vis_faces_keep[, 1], 2],
              proj$xy[vis_faces_keep[, 2], 2],
              proj$xy[vis_faces_keep[, 3], 2]),
        face_id = rep(fids, 3),
        panel = panel_label,
        overlay_value = rep(vals_keep, 3)
      )
      poly_list[[length(poly_list) + 1L]] <- chunk
    }
  }

  list(
    polygons = dplyr::bind_rows(poly_list),
    boundaries = dplyr::bind_rows(boundary_list)
  )
}


#' Map numeric overlay values to hex colours
#' @keywords internal
#' @noRd
.overlay_value_to_hex <- function(vals, palette = "vik", lim = NULL) {
  vals <- as.numeric(vals)
  finite <- is.finite(vals)
  out <- rep(NA_character_, length(vals))
  if (!any(finite)) return(out)

  if (is.null(lim)) {
    lim <- range(vals[finite], na.rm = TRUE)
    if (!all(is.finite(lim))) return(out)
    if (lim[1] == lim[2]) {
      lim <- lim + c(-1e-6, 1e-6)
    }
  }

  pal <- scico::scico(256, palette = palette)
  v <- vals
  v[v < lim[1]] <- lim[1]
  v[v > lim[2]] <- lim[2]
  idx <- floor((v - lim[1]) / (lim[2] - lim[1]) * 255) + 1
  idx[!finite] <- NA_integer_
  out[finite] <- pal[idx[finite]]
  out
}


#' Compute panel-wise layout transforms for projected brain polygons
#'
#' @param poly_data Polygon data with columns \code{panel}, \code{view},
#'   \code{x}, \code{y}.
#' @param panel_layout Character scalar: \code{"native"} or
#'   \code{"presentation"}.
#' @return A tibble with per-panel transform parameters, or \code{NULL}.
#' @keywords internal
#' @noRd
.compute_panel_layout_transforms <- function(poly_data,
                                             panel_layout = c("native",
                                                              "presentation")) {
  panel_layout <- match.arg(panel_layout)
  if (panel_layout == "native") return(NULL)
  if (is.null(poly_data) || nrow(poly_data) == 0) return(NULL)

  needed <- c("panel", "view", "x", "y")
  if (!all(needed %in% names(poly_data))) return(NULL)

  ext <- poly_data |>
    dplyr::group_by(panel, view) |>
    dplyr::summarise(
      xmin = min(x),
      xmax = max(x),
      ymin = min(y),
      ymax = max(y),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      cx = (xmin + xmax) / 2,
      cy = (ymin + ymax) / 2,
      width = xmax - xmin,
      height = ymax - ymin,
      rotate = view %in% c("dorsal", "ventral"),
      width_rot = dplyr::if_else(rotate, height, width),
      height_rot = dplyr::if_else(rotate, width, height)
    )

  span <- pmax(ext$width_rot, ext$height_rot)
  span[!is.finite(span) | span <= 0] <- 1
  ext$scale <- 1 / span

  ext[, c("panel", "cx", "cy", "rotate", "scale")]
}

#' Apply panel-wise transforms to point geometry
#'
#' @param dat Data frame with columns \code{panel}, \code{x}, \code{y}.
#' @param transforms Output of [.compute_panel_layout_transforms()].
#' @param x_col Name of x column.
#' @param y_col Name of y column.
#' @return Transformed data frame.
#' @keywords internal
#' @noRd
.apply_panel_layout_to_points <- function(dat, transforms,
                                          x_col = "x", y_col = "y") {
  if (is.null(dat) || nrow(dat) == 0) return(dat)
  if (is.null(transforms) || nrow(transforms) == 0) return(dat)
  if (!all(c("panel", x_col, y_col) %in% names(dat))) return(dat)

  out <- dplyr::left_join(
    dat,
    transforms,
    by = "panel"
  )

  cx <- out$cx
  cy <- out$cy
  rotate <- out$rotate
  scale <- out$scale

  missing_t <- is.na(cx) | is.na(cy) | is.na(scale)
  cx[missing_t] <- 0
  cy[missing_t] <- 0
  rotate[is.na(rotate)] <- FALSE
  scale[missing_t] <- 1

  x0 <- out[[x_col]] - cx
  y0 <- out[[y_col]] - cy

  x1 <- ifelse(rotate, -y0, x0)
  y1 <- ifelse(rotate, x0, y0)

  out[[x_col]] <- x1 * scale
  out[[y_col]] <- y1 * scale

  out$cx <- NULL
  out$cy <- NULL
  out$rotate <- NULL
  out$scale <- NULL
  out
}

#' Apply panel-wise transforms to segment geometry
#'
#' @param dat Data frame with columns \code{panel}, \code{x}, \code{y},
#'   \code{xend}, \code{yend}.
#' @param transforms Output of [.compute_panel_layout_transforms()].
#' @return Transformed data frame.
#' @keywords internal
#' @noRd
.apply_panel_layout_to_segments <- function(dat, transforms) {
  if (is.null(dat) || nrow(dat) == 0) return(dat)
  if (is.null(transforms) || nrow(transforms) == 0) return(dat)
  if (!all(c("panel", "x", "y", "xend", "yend") %in% names(dat))) return(dat)

  out <- dplyr::left_join(
    dat,
    transforms,
    by = "panel"
  )

  cx <- out$cx
  cy <- out$cy
  rotate <- out$rotate
  scale <- out$scale

  missing_t <- is.na(cx) | is.na(cy) | is.na(scale)
  cx[missing_t] <- 0
  cy[missing_t] <- 0
  rotate[is.na(rotate)] <- FALSE
  scale[missing_t] <- 1

  apply_xy <- function(x, y) {
    x0 <- x - cx
    y0 <- y - cy
    x1 <- ifelse(rotate, -y0, x0)
    y1 <- ifelse(rotate, x0, y0)
    list(x = x1 * scale, y = y1 * scale)
  }

  p1 <- apply_xy(out$x, out$y)
  p2 <- apply_xy(out$xend, out$yend)

  out$x <- p1$x
  out$y <- p1$y
  out$xend <- p2$x
  out$yend <- p2$y

  out$cx <- NULL
  out$cy <- NULL
  out$rotate <- NULL
  out$scale <- NULL
  out
}


#' Plot Brain Surface Atlas
#'
#' @description
#' Renders a triangle-mesh projection of cortical surface parcellations with
#' configurable views and optional ggiraph interactivity. This function replaces
#' the legacy ggseg-based visualisation pipeline.
#'
#' @param surfatlas A surface atlas object of class \code{"surfatlas"}
#'   (e.g. from \code{\link{schaefer_surf}()} or \code{\link{glasser_surf}()}).
#' @param vals Optional numeric vector of values to map onto parcels. Length
#'   must equal the number of atlas regions (\code{length(surfatlas$ids)}).
#'   When \code{NULL} (default), parcels are coloured using the ROI colour
#'   system.
#' @param views Character vector of views to render. Any combination of
#'   \code{"lateral"}, \code{"medial"}, \code{"dorsal"}, \code{"ventral"}.
#'   Default: \code{c("lateral", "medial")}.
#' @param hemis Character vector of hemispheres to include.
#'   Default: \code{c("left", "right")}.
#' @param surface Surface type. One of \code{"inflated"}, \code{"pial"},
#'   \code{"white"}. Must match the surface type of \code{surfatlas}.
#' @param color_method Colour algorithm for discrete parcel colouring (when
#'   \code{vals} is \code{NULL}). Passed to \code{\link{atlas_roi_colors}()}.
#'   Default: \code{"rule_hcl"}.
#' @param colors Optional pre-computed colours: a tibble with \code{id} and
#'   \code{color} columns, or a named character vector of hex colours keyed
#'   by region ID. Overrides \code{color_method} when \code{vals} is
#'   \code{NULL}.
#' @param palette Character: scico palette for continuous colour scale (when
#'   \code{vals} is provided). Default: \code{"cork"}.
#' @param lim Numeric vector of length 2 for colour scale limits (continuous
#'   mode). Defaults to range of \code{vals}.
#' @param interactive Logical. If \code{TRUE} (default), returns a
#'   \code{ggiraph::girafe} widget with hover tooltips. If \code{FALSE},
#'   returns a static \code{ggplot2} object.
#' @param data_id_mode Interactive data-id granularity (when
#'   \code{interactive = TRUE}): \code{"parcel"} (default) uses parcel ids;
#'   \code{"polygon"} encodes panel + parcel + polygon/face id for
#'   click-to-surface workflows.
#' @param ncol Integer: number of columns in the facet layout. Default: 2.
#' @param panel_layout Panel coordinate layout strategy:
#'   \code{"native"} (default) preserves raw projected units;
#'   \code{"presentation"} recentres each panel, rotates dorsal/ventral views
#'   to horizontal, and normalises per-panel scale for a cleaner ggseg-like
#'   grid.
#' @param style Visual preset. \code{"default"} keeps existing behaviour.
#'   \code{"ggseg_like"} enables a cleaner publication style and, unless
#'   explicitly overridden, switches \code{panel_layout} to
#'   \code{"presentation"} with softer border defaults and light projection
#'   smoothing.
#' @param border Logical. If \code{TRUE} (default), draw thin lines at parcel
#'   boundaries (edges between different parcels). Gives a clean ggseg-like
#'   appearance.
#' @param border_geom Boundary rendering method. \code{"path"} (default) chains
#'   boundary edges into longer paths for smoother lines; \code{"segment"} draws
#'   each boundary edge independently.
#' @param boundary_smooth Non-negative integer controlling Chaikin smoothing
#'   iterations applied to boundary paths when \code{border_geom = "path"}.
#'   \code{0} (default) keeps original mesh-aligned boundaries; \code{1} or
#'   \code{2} yields cleaner curves in close-up figures.
#' @param projection_smooth Non-negative integer controlling Laplacian-like
#'   smoothing iterations applied to projected vertex coordinates before parcel
#'   polygons are constructed. This smooths filled parcel edges while
#'   preserving shared boundaries across parcels. \code{0} (default) keeps raw
#'   projected coordinates.
#' @param border_color Colour for parcel boundary lines. Default:
#'   \code{"grey30"}.
#' @param border_size Line width for parcel boundaries. Default: \code{0.15}.
#' @param border_lineend Line end style for boundary lines (passed to
#'   \code{\link[ggplot2]{geom_path}} / \code{\link[ggplot2]{geom_segment}}).
#'   One of \code{"butt"},
#'   \code{"round"}, \code{"square"}. Default: \code{"round"}.
#' @param border_linejoin Line join style for boundary lines (passed to
#'   \code{\link[ggplot2]{geom_path}} / \code{\link[ggplot2]{geom_segment}}).
#'   One of \code{"round"},
#'   \code{"mitre"}, \code{"bevel"}. Default: \code{"round"}.
#' @param silhouette Logical. If \code{TRUE}, draw the mesh silhouette (edges
#'   between visible and culled faces) as a separate boundary layer. Defaults
#'   to \code{border}.
#' @param silhouette_color Colour for silhouette lines. Default:
#'   \code{border_color}.
#' @param silhouette_size Line width for silhouette lines. Default:
#'   \code{border_size}.
#' @param network_border Logical. If \code{TRUE}, highlight boundaries between
#'   different networks (requires \code{surfatlas$network}). Default:
#'   \code{FALSE}.
#' @param network_border_color Colour for network boundary lines. Default:
#'   \code{border_color}.
#' @param network_border_size Line width for network boundary lines. Default:
#'   \code{border_size * 2}.
#' @param shading Logical. If \code{TRUE}, overlay a subtle normal-based shading
#'   layer to enhance depth cues (recommended for static figures).
#' @param shading_strength Numeric in \code{[0, 1]}. Maximum opacity of the
#'   shading overlay. Default: \code{0.22}.
#' @param shading_gamma Positive numeric scalar controlling the shadow falloff.
#'   Higher values concentrate shadows in more oblique regions. Default:
#'   \code{1}.
#' @param shading_color Colour of the shading overlay. Default: \code{"black"}.
#' @param fill_alpha Numeric in \code{[0, 1]}. Opacity of parcel fills.
#'   Lower values can help the shading read more clearly. Default: \code{1}.
#' @param overlay Vertex-wise overlay or a \code{NeuroVol}.
#'   If a \code{NeuroVol}, it is automatically projected onto the surface
#'   using \code{neurosurf::vol_to_surf()}.
#'   Otherwise, a list with \code{lh} and \code{rh} components (numeric
#'   vectors matching the vertex count of each hemisphere mesh).
#' @param overlay_threshold Optional absolute threshold for overlay values
#'   before rendering.
#' @param overlay_alpha Numeric in \code{[0, 1]}. Opacity of overlay polygons.
#'   Default: \code{0.45}.
#' @param overlay_palette scico palette for overlay colour mapping. Default:
#'   \code{"vik"}.
#' @param overlay_lim Optional numeric length-2 limits for overlay colour
#'   mapping.
#' @param overlay_fun Character: interpolation function passed to
#'   \code{neurosurf::vol_to_surf()} when \code{overlay} is a
#'   \code{NeuroVol}. One of \code{"avg"}, \code{"nn"}, or \code{"mode"}.
#'   Default: \code{"avg"}.
#' @param overlay_sampling Character: sampling strategy passed to
#'   \code{neurosurf::vol_to_surf()} when \code{overlay} is a
#'   \code{NeuroVol}. One of \code{"midpoint"}, \code{"normal_line"}, or
#'   \code{"thickness"}. Default: \code{"midpoint"}.
#' @param overlay_border Logical. If \code{TRUE}, draw cluster overlay
#'   boundaries. Default: \code{TRUE}.
#' @param overlay_border_color Colour for overlay boundaries. Default:
#'   \code{"black"}.
#' @param overlay_border_size Line width for overlay boundaries. Default:
#'   \code{0.25}.
#' @param outline Logical. If \code{TRUE}, draw every triangle edge (mesh
#'   wireframe). Default: \code{FALSE}. Typically \code{border} is preferred.
#' @param bg Character: background colour for the plot. Default: \code{"white"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{ggplot2} object (when \code{interactive = FALSE}) or a
#'   \code{ggiraph::girafe} widget (when \code{interactive = TRUE}).
#'
#' @examples
#' \dontrun{
#' atl <- schaefer_surf(200, 17)
#' plot_brain(atl)
#' plot_brain(atl, vals = rnorm(200), palette = "vik")
#' plot_brain(atl, views = "lateral", interactive = FALSE)
#'
#' # Styling: rounded white parcel borders + thicker silhouette + network edges
#' plot_brain(
#'   atl,
#'   interactive = FALSE,
#'   border_color = "white",
#'   border_size = 0.25,
#'   border_lineend = "round",
#'   silhouette_size = 0.6,
#'   network_border = TRUE,
#'   network_border_color = "grey10",
#'   network_border_size = 0.5,
#'   shading = TRUE,
#'   fill_alpha = 0.98,
#'   bg = "#f7f7f7"
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes after_scale facet_wrap theme_void theme
#'   element_rect element_text scale_fill_identity scale_colour_identity labs
#'   coord_equal margin
#' @importFrom ggplot2 geom_segment
#' @importFrom ggiraph geom_polygon_interactive girafe opts_tooltip opts_hover
#'   opts_hover_inv opts_selection opts_sizing
#' @importFrom scico scale_fill_scico
#' @importFrom scales squish
#' @importFrom tibble tibble
#' @importFrom dplyr left_join bind_rows
#' @importFrom memoise memoise
#' @export
plot_brain <- function(surfatlas,
                       vals = NULL,
                       views = c("lateral", "medial"),
                       hemis = c("left", "right"),
                       surface = "inflated",
                       color_method = "rule_hcl",
                       colors = NULL,
                       palette = "cork",
                       lim = NULL,
                       interactive = TRUE,
                       data_id_mode = c("parcel", "polygon"),
                       ncol = 2L,
                       panel_layout = c("native", "presentation"),
                       style = c("default", "ggseg_like"),
                       border = TRUE,
                       border_geom = c("path", "segment"),
                       boundary_smooth = 0L,
                       projection_smooth = 0L,
                       border_color = "grey30",
                       border_size = 0.15,
                       border_lineend = "round",
                       border_linejoin = "round",
                       silhouette = border,
                       silhouette_color = border_color,
                       silhouette_size = border_size,
                       network_border = FALSE,
                       network_border_color = border_color,
                       network_border_size = border_size * 2,
                       shading = FALSE,
                       shading_strength = 0.22,
                       shading_gamma = 1,
                       shading_color = "black",
                       fill_alpha = 1,
                       overlay = NULL,
                       overlay_threshold = NULL,
                       overlay_alpha = 0.45,
                       overlay_palette = "vik",
                       overlay_lim = NULL,
                       overlay_border = TRUE,
                       overlay_border_color = "black",
                       overlay_border_size = 0.25,
                       overlay_fun = c("avg", "nn", "mode"),
                       overlay_sampling = c("midpoint", "normal_line",
                                            "thickness"),
                       outline = FALSE,
                       bg = "white",
                       ...) {
  style <- match.arg(style)

  panel_layout_missing <- missing(panel_layout)
  border_color_missing <- missing(border_color)
  border_size_missing <- missing(border_size)
  boundary_smooth_missing <- missing(boundary_smooth)
  projection_smooth_missing <- missing(projection_smooth)
  silhouette_missing <- missing(silhouette)
  silhouette_color_missing <- missing(silhouette_color)
  network_border_missing <- missing(network_border)
  shading_missing <- missing(shading)
  bg_missing <- missing(bg)

  if (identical(style, "ggseg_like")) {
    if (panel_layout_missing) panel_layout <- "presentation"
    if (boundary_smooth_missing) boundary_smooth <- 2L
    if (projection_smooth_missing) projection_smooth <- 1L
    if (border_color_missing) border_color <- "grey82"
    if (border_size_missing) border_size <- 0.24
    if (silhouette_missing) silhouette <- FALSE
    if (silhouette_color_missing) silhouette_color <- border_color
    if (network_border_missing) network_border <- FALSE
    if (shading_missing) shading <- FALSE
    if (bg_missing) bg <- "#f7f7f7"
  }

  # Input validation
  if (!inherits(surfatlas, "surfatlas")) {
    stop("'surfatlas' must be a surface atlas object of class 'surfatlas'.\n",
         "Use schaefer_surf() or glasser_surf() to create one.",
         call. = FALSE)
  }

  views <- match.arg(views, c("lateral", "medial", "dorsal", "ventral"),
                     several.ok = TRUE)
  hemis <- match.arg(hemis, c("left", "right"), several.ok = TRUE)
  data_id_mode <- match.arg(data_id_mode)
  panel_layout <- match.arg(panel_layout)
  border_geom <- match.arg(border_geom)
  if (!is.numeric(boundary_smooth) || length(boundary_smooth) != 1 ||
      is.na(boundary_smooth) || boundary_smooth < 0 ||
      boundary_smooth != as.integer(boundary_smooth)) {
    stop("'boundary_smooth' must be a non-negative integer scalar.",
         call. = FALSE)
  }
  boundary_smooth <- as.integer(boundary_smooth)
  if (!is.numeric(projection_smooth) || length(projection_smooth) != 1 ||
      is.na(projection_smooth) || projection_smooth < 0 ||
      projection_smooth != as.integer(projection_smooth)) {
    stop("'projection_smooth' must be a non-negative integer scalar.",
         call. = FALSE)
  }
  projection_smooth <- as.integer(projection_smooth)

  border_lineend <- match.arg(border_lineend, c("butt", "round", "square"))
  border_linejoin <- match.arg(border_linejoin, c("round", "mitre", "bevel"))

  if (!is.numeric(fill_alpha) || length(fill_alpha) != 1 ||
      is.na(fill_alpha) || fill_alpha < 0 || fill_alpha > 1) {
    stop("'fill_alpha' must be a numeric scalar in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(overlay_alpha) || length(overlay_alpha) != 1 ||
      is.na(overlay_alpha) || overlay_alpha < 0 || overlay_alpha > 1) {
    stop("'overlay_alpha' must be a numeric scalar in [0, 1].", call. = FALSE)
  }

  if (!is.numeric(shading_strength) || length(shading_strength) != 1 ||
      is.na(shading_strength) || shading_strength < 0 || shading_strength > 1) {
    stop("'shading_strength' must be a numeric scalar in [0, 1].", call. = FALSE)
  }

  if (!is.numeric(shading_gamma) || length(shading_gamma) != 1 ||
      is.na(shading_gamma) || shading_gamma <= 0) {
    stop("'shading_gamma' must be a positive numeric scalar.", call. = FALSE)
  }

  if (!is.null(vals)) {
    if (length(vals) != length(surfatlas$ids)) {
      stop("Length of 'vals' (", length(vals), ") must match number of atlas ",
           "regions (", length(surfatlas$ids), ").",
           call. = FALSE)
    }
    if (!is.null(lim)) {
      stopifnot(is.numeric(lim), length(lim) == 2)
    } else {
      lim <- range(vals, na.rm = TRUE)
    }
  }

  # Build polygon data (memoised for speed).
  # Use merged parcel polygons (~800 SVG elements) unless outline mode
  # requests the full triangle mesh (~160k elements).
  if (outline) {
    build_result <- .build_brain_polygon_data_memo(
      surfatlas,
      views,
      surface,
      projection_smooth = projection_smooth
    )
  } else {
    build_result <- .build_merged_polygon_data_memo(
      surfatlas,
      views,
      surface,
      projection_smooth = projection_smooth
    )
  }
  poly_data <- build_result$polygons
  boundary_data <- build_result$boundaries

  # Filter to requested hemispheres
  poly_data <- poly_data[poly_data$hemi %in% hemis, , drop = FALSE]
  if (!is.null(boundary_data) && nrow(boundary_data) > 0) {
    boundary_data <- boundary_data[boundary_data$panel %in% poly_data$panel, ,
                                   drop = FALSE]
    if (!"edge_type" %in% names(boundary_data)) {
      boundary_data$edge_type <- "parcel"
    }
  }

  if (nrow(poly_data) == 0) {
    stop("No polygon data generated. Check views and hemis arguments.",
         call. = FALSE)
  }

  # Merged polygons use poly_id; triangle mode uses face_id.
  # This controls both grouping and optional polygon-level interactive ids.
  group_col <- if (outline) "face_id" else "poly_id"

  # Optional panel-wise layout normalisation for presentation use.
  panel_transforms <- .compute_panel_layout_transforms(
    poly_data,
    panel_layout = panel_layout
  )
  if (!is.null(panel_transforms)) {
    poly_data <- .apply_panel_layout_to_points(poly_data, panel_transforms)
    if (!is.null(boundary_data) && nrow(boundary_data) > 0) {
      boundary_data <- .apply_panel_layout_to_segments(boundary_data,
                                                       panel_transforms)
    }
  }

  # --- Colour assignment ---
  if (is.null(vals)) {
    # Discrete parcel colouring
    if (is.null(colors)) {
      # surfatlas objects lack a NeuroVol, so atlas_roi_colors() (which needs
      # voxel centroids) won't work.  Use the bundled cmap instead.
      if (!is.null(surfatlas$cmap)) {
        cmap <- surfatlas$cmap
        hex <- grDevices::rgb(cmap[[1]], cmap[[2]], cmap[[3]], maxColorValue = 255)
        color_tbl <- tibble::tibble(id = surfatlas$ids, color = hex)
      } else {
        # Fallback: generate simple HCL palette
        n <- length(surfatlas$ids)
        hex <- grDevices::hcl(h = seq(15, 375, length.out = n + 1)[seq_len(n)],
                              c = 70, l = 65, fixup = TRUE)
        color_tbl <- tibble::tibble(id = surfatlas$ids, color = hex)
      }
    } else if (is.data.frame(colors)) {
      color_tbl <- colors
    } else if (is.character(colors)) {
      if (!is.null(names(colors))) {
        color_tbl <- tibble::tibble(id = as.integer(names(colors)),
                                    color = unname(colors))
      } else {
        color_tbl <- tibble::tibble(id = surfatlas$ids, color = colors)
      }
    } else {
      stop("'colors' must be NULL, a tibble, or a character vector of hex colours",
           call. = FALSE)
    }

    color_map <- stats::setNames(color_tbl$color, as.character(color_tbl$id))
    color_map["0"] <- bg

    poly_data$fill_color <- color_map[as.character(poly_data$parcel_id)]
    poly_data$fill_color[is.na(poly_data$fill_color)] <- bg
    use_identity_fill <- TRUE
  } else {
    # Continuous value mapping
    val_map <- stats::setNames(vals, as.character(surfatlas$ids))
    poly_data$fill_value <- val_map[as.character(poly_data$parcel_id)]
    use_identity_fill <- FALSE
  }

  # --- Tooltip text ---
  if (!is.null(vals)) {
    val_lookup <- stats::setNames(vals, as.character(surfatlas$ids))
    poly_data$tooltip <- paste0(
      poly_data$label, "\n",
      "Value: ", round(val_lookup[as.character(poly_data$parcel_id)], 3)
    )
  } else {
    poly_data$tooltip <- poly_data$label
  }
  poly_data$data_id <- if (identical(data_id_mode, "parcel")) {
    as.character(poly_data$parcel_id)
  } else {
    .encode_plot_brain_data_id(
      panel = poly_data$panel,
      parcel_id = poly_data$parcel_id,
      shape_id = poly_data[[group_col]]
    )
  }

  # Order panels for faceting
  panel_levels <- c()
  for (v in views) {
    for (h in hemis) {
      panel_levels <- c(panel_levels,
                        paste0(tools::toTitleCase(h), " ", tools::toTitleCase(v)))
    }
  }
  poly_data$panel <- factor(poly_data$panel, levels = panel_levels)

  # --- Build plot ---
  # When outline is disabled, paint polygon borders the same colour as fill
  # to eliminate anti-aliasing seams between adjacent same-parcel triangles.
  poly_lwd <- if (outline) 0.1 else 0.15

  p <- ggplot2::ggplot(poly_data,
                       ggplot2::aes(x = x, y = y, group = .data[[group_col]]))

  # Build fixed geom params — only include colour when outline is on
  geom_params <- list(linewidth = poly_lwd, alpha = fill_alpha)
  if (outline) geom_params$colour <- "grey40"

  # Choose fill column
  fill_sym <- if (use_identity_fill) "fill_color" else "fill_value"

  # Build aesthetic mapping
  if (outline) {
    if (interactive) {
      geom_aes <- ggplot2::aes(fill = .data[[fill_sym]],
                                tooltip = tooltip, data_id = data_id)
    } else {
      geom_aes <- ggplot2::aes(fill = .data[[fill_sym]])
    }
  } else {
    if (interactive) {
      geom_aes <- ggplot2::aes(fill = .data[[fill_sym]],
                                colour = ggplot2::after_scale(fill),
                                tooltip = tooltip, data_id = data_id)
    } else {
      geom_aes <- ggplot2::aes(fill = .data[[fill_sym]],
                                colour = ggplot2::after_scale(fill))
    }
  }

  # Add geom layer
  geom_fn <- if (interactive) {
    ggiraph::geom_polygon_interactive
  } else {
    ggplot2::geom_polygon
  }
  p <- p + do.call(geom_fn, c(list(mapping = geom_aes), geom_params))

  # Add fill scale
  if (use_identity_fill) {
    p <- p + ggplot2::scale_fill_identity()
  } else {
    p <- p + scico::scale_fill_scico(
      palette = palette, limits = lim, oob = scales::squish, na.value = bg
    )
  }

  # Colour identity scale needed when borders match fill via after_scale()
  if (!outline) {
    p <- p + ggplot2::scale_colour_identity()
  }

  # Optional projected cluster overlay
  if (!is.null(overlay)) {
    if (inherits(overlay, "NeuroVol")) {
      overlay_fun <- match.arg(overlay_fun)
      overlay_sampling <- match.arg(overlay_sampling)
      proj <- .project_cluster_overlay(
        cluster_vol = overlay,
        surfatlas = surfatlas,
        fun = overlay_fun,
        sampling = overlay_sampling
      )
      overlay <- proj$overlay
    }
    if (!is.list(overlay)) {
      stop("'overlay' must be a NeuroVol, or a list with 'lh'/'rh' vertex vectors.",
           call. = FALSE)
    }

    ov_build <- .build_overlay_polygon_data(
      surfatlas = surfatlas,
      overlay = overlay,
      views = views,
      hemis = hemis,
      projection_smooth = projection_smooth,
      threshold = overlay_threshold
    )

    ov_poly <- ov_build$polygons
    ov_bnd <- ov_build$boundaries

    if (!is.null(ov_poly) && nrow(ov_poly) > 0) {
      if (!is.null(panel_transforms)) {
        ov_poly <- .apply_panel_layout_to_points(ov_poly, panel_transforms)
      }
      ov_poly$panel <- factor(ov_poly$panel, levels = levels(poly_data$panel))
      ov_poly$overlay_color <- .overlay_value_to_hex(
        ov_poly$overlay_value,
        palette = overlay_palette,
        lim = overlay_lim
      )

      p <- p + ggplot2::geom_polygon(
        data = ov_poly,
        ggplot2::aes(
          x = x, y = y, group = face_id,
          fill = I(overlay_color)
        ),
        alpha = overlay_alpha,
        colour = NA,
        inherit.aes = FALSE
      )

      if (isTRUE(overlay_border) && !is.null(ov_bnd) && nrow(ov_bnd) > 0) {
        if (!is.null(panel_transforms)) {
          ov_bnd <- .apply_panel_layout_to_segments(ov_bnd, panel_transforms)
        }
        ov_bnd$panel <- factor(ov_bnd$panel, levels = levels(poly_data$panel))
        ov_paths <- NULL
        if (border_geom == "path") {
          ov_paths <- .boundary_edges_to_paths(ov_bnd)
          if (!is.null(ov_paths) && nrow(ov_paths) > 0) {
            if (boundary_smooth > 0L) {
              ov_paths <- .smooth_boundary_paths(
                ov_paths,
                n_iter = boundary_smooth
              )
            }
            ov_paths$panel <- factor(ov_paths$panel,
                                     levels = levels(poly_data$panel))
          }
        }

        if (!is.null(ov_paths) && nrow(ov_paths) > 0) {
          ov_paths <- ov_paths[order(ov_paths$path_id, ov_paths$vertex_order), ,
                               drop = FALSE]
          p <- p + ggplot2::geom_path(
            data = ov_paths,
            ggplot2::aes(x = x, y = y, group = path_id),
            colour = overlay_border_color,
            linewidth = overlay_border_size,
            lineend = "round",
            linejoin = "round",
            inherit.aes = FALSE
          )
        } else {
          p <- p + ggplot2::geom_segment(
            data = ov_bnd,
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            colour = overlay_border_color,
            linewidth = overlay_border_size,
            lineend = "round",
            linejoin = "round",
            inherit.aes = FALSE
          )
        }
      }
    }
  }

  # Optional shading overlay (static polygons; uses triangle mesh data)
  if (isTRUE(shading) && !is.null(shading_strength) && shading_strength > 0) {
    if (!outline) {
      shade_build <- .build_brain_polygon_data_memo(surfatlas, views, surface)
      shade_data <- shade_build$polygons
      shade_data <- shade_data[shade_data$hemi %in% hemis, , drop = FALSE]
      shade_data <- shade_data[shade_data$panel %in% levels(poly_data$panel), ,
                               drop = FALSE]
    } else {
      shade_data <- poly_data
    }

    if (!is.null(shade_data) && nrow(shade_data) > 0 &&
        "shade" %in% names(shade_data)) {
      if (!is.null(panel_transforms)) {
        shade_data <- .apply_panel_layout_to_points(shade_data, panel_transforms)
      }
      shade_data$panel <- factor(shade_data$panel,
                                 levels = levels(poly_data$panel))

      shade_data$alpha <- shading_strength *
        (pmax(0, 1 - shade_data$shade) ^ shading_gamma)

      p <- p +
        ggplot2::geom_polygon(
          data = shade_data,
          ggplot2::aes(x = x, y = y, group = face_id, alpha = alpha),
          fill = shading_color,
          colour = NA,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_alpha_identity(guide = "none")
    }
  }

  # Add parcel boundary lines (non-interactive, unaffected by hover dimming)
  if (!is.null(boundary_data) && nrow(boundary_data) > 0) {
    boundary_data$panel <- factor(boundary_data$panel,
                                  levels = levels(poly_data$panel))

    boundary_paths <- NULL
    if (border_geom == "path") {
      boundary_paths <- .boundary_edges_to_paths(boundary_data)
      if (!is.null(boundary_paths) && nrow(boundary_paths) > 0) {
        if (boundary_smooth > 0L) {
          boundary_paths <- .smooth_boundary_paths(
            boundary_paths,
            n_iter = boundary_smooth
          )
        }
        boundary_paths$panel <- factor(boundary_paths$panel,
                                       levels = levels(poly_data$panel))
      }
    }

    if (border) {
      if (!is.null(boundary_paths) && nrow(boundary_paths) > 0) {
        border_data <- boundary_paths[boundary_paths$edge_type %in%
                                        c("parcel", "network"), ,
                                      drop = FALSE]
        if (nrow(border_data) > 0) {
          border_data <- border_data[order(border_data$path_id,
                                           border_data$vertex_order), ,
                                     drop = FALSE]
          p <- p + ggplot2::geom_path(
            data = border_data,
            ggplot2::aes(x = x, y = y, group = path_id),
            colour = border_color,
            linewidth = border_size,
            lineend = border_lineend,
            linejoin = border_linejoin,
            inherit.aes = FALSE
          )
        }
      } else {
        border_data <- boundary_data[boundary_data$edge_type %in%
                                       c("parcel", "network"), ,
                                     drop = FALSE]
        if (nrow(border_data) > 0) {
          p <- p + ggplot2::geom_segment(
            data = border_data,
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            colour = border_color,
            linewidth = border_size,
            lineend = border_lineend,
            linejoin = border_linejoin,
            inherit.aes = FALSE
          )
        }
      }
    }

    if (network_border) {
      if (!is.null(boundary_paths) && nrow(boundary_paths) > 0) {
        net_data <- boundary_paths[boundary_paths$edge_type == "network", ,
                                  drop = FALSE]
        if (nrow(net_data) > 0) {
          net_data <- net_data[order(net_data$path_id,
                                     net_data$vertex_order), ,
                               drop = FALSE]
          p <- p + ggplot2::geom_path(
            data = net_data,
            ggplot2::aes(x = x, y = y, group = path_id),
            colour = network_border_color,
            linewidth = network_border_size,
            lineend = border_lineend,
            linejoin = border_linejoin,
            inherit.aes = FALSE
          )
        }
      } else {
        net_data <- boundary_data[boundary_data$edge_type == "network", ,
                                  drop = FALSE]
        if (nrow(net_data) > 0) {
          p <- p + ggplot2::geom_segment(
            data = net_data,
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            colour = network_border_color,
            linewidth = network_border_size,
            lineend = border_lineend,
            linejoin = border_linejoin,
            inherit.aes = FALSE
          )
        }
      }
    }

    if (silhouette) {
      if (!is.null(boundary_paths) && nrow(boundary_paths) > 0) {
        sil_data <- boundary_paths[boundary_paths$edge_type == "silhouette", ,
                                  drop = FALSE]
        if (nrow(sil_data) > 0) {
          sil_data <- sil_data[order(sil_data$path_id,
                                     sil_data$vertex_order), ,
                               drop = FALSE]
          p <- p + ggplot2::geom_path(
            data = sil_data,
            ggplot2::aes(x = x, y = y, group = path_id),
            colour = silhouette_color,
            linewidth = silhouette_size,
            lineend = border_lineend,
            linejoin = border_linejoin,
            inherit.aes = FALSE
          )
        }
      } else {
        sil_data <- boundary_data[boundary_data$edge_type == "silhouette", ,
                                  drop = FALSE]
        if (nrow(sil_data) > 0) {
          p <- p + ggplot2::geom_segment(
            data = sil_data,
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            colour = silhouette_color,
            linewidth = silhouette_size,
            lineend = border_lineend,
            linejoin = border_linejoin,
            inherit.aes = FALSE
          )
        }
      }
    }
  }

  p <- p +
    ggplot2::facet_wrap(~ panel, ncol = ncol) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      strip.text = ggplot2::element_text(size = 11, face = "bold",
                                         margin = ggplot2::margin(b = 4))
    ) +
    ggplot2::labs(fill = NULL)

  if (!interactive) {
    return(p)
  }

  ggiraph::girafe(
    ggobj = p,
    width_svg = 8,
    height_svg = max(4, 2 * ceiling(length(panel_levels) / ncol)),
    options = list(
      ggiraph::opts_tooltip(
        opacity = 0.85,
        css = "font-family: sans-serif; font-size: 12px; padding: 6px 10px;"
      ),
      ggiraph::opts_hover(
        css = paste0(
          "stroke:#FFD700; stroke-width:1.5px; stroke-opacity:1; ",
          "stroke-linecap:round; stroke-linejoin:round;"
        )
      ),
      ggiraph::opts_hover_inv(
        css = "opacity:0.25;"
      ),
      ggiraph::opts_selection(
        css = paste0(
          "stroke:#FF4500; stroke-width:2px; stroke-opacity:1; ",
          "stroke-linecap:round; stroke-linejoin:round;"
        ),
        type = "single",
        only_shiny = FALSE
      ),
      ggiraph::opts_sizing(rescale = TRUE, width = 0.9)
    )
  )
}
