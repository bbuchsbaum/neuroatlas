# Geometry primitives used by plot_brain().
#
# This file collects the pure-geometry helpers that underpin
# plot_brain() rendering: face-normal computation, face-level parcel
# assignment, boundary-edge extraction, Chaikin corner-cutting, parcel
# polygon chaining, 3D -> 2D view projection, vertex-adjacency Laplacian
# smoothing, and small utility helpers. These functions don't know
# anything about surfatlas / ggplot state; they take plain numeric
# matrices and vectors and return plain tibbles or matrices. Keeping
# them in their own file makes plot_brain.R easier to read and lets the
# primitives be tested in isolation.
#
# Split out from plot_brain.R; behaviour is intentionally unchanged.

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
