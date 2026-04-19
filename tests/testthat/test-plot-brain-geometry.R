# Tests for the pure geometry primitives extracted into
# R/plot_brain_geometry.R. These exercise the helpers directly on tiny
# synthetic meshes so regressions in plot_brain() rendering math surface
# via unit tests rather than through visual diffs of full brain plots.

# ---- Helpers ----------------------------------------------------------------

# Two-triangle square in the y-z plane, z-normal pointing +x.
.square_mesh <- function() {
  verts <- rbind(
    c(0, 0, 0),
    c(0, 1, 0),
    c(0, 1, 1),
    c(0, 0, 1)
  )
  faces <- rbind(
    c(1L, 2L, 3L),
    c(1L, 3L, 4L)
  )
  list(verts = verts, faces = faces)
}


# ---- .face_normals ----------------------------------------------------------

test_that(".face_normals returns unit normals with the expected direction", {
  sq <- .square_mesh()
  fn <- .face_normals(sq$verts, sq$faces)

  expect_equal(nrow(fn), nrow(sq$faces))
  expect_equal(ncol(fn), 3L)
  expect_equal(sqrt(rowSums(fn^2)), c(1, 1), tolerance = 1e-10)
  # Both triangles wound counter-clockwise as viewed from +x: normals point +x
  expect_true(all(fn[, 1] > 0))
  expect_equal(abs(fn[, 2]), c(0, 0), tolerance = 1e-10)
  expect_equal(abs(fn[, 3]), c(0, 0), tolerance = 1e-10)
})

test_that(".face_normals handles degenerate zero-area triangles without NaN", {
  verts <- rbind(c(0, 0, 0), c(1, 0, 0), c(1, 0, 0))
  faces <- rbind(c(1L, 2L, 3L))
  fn <- .face_normals(verts, faces)
  expect_false(any(is.nan(fn)))
})


# ---- .face_parcel_ids -------------------------------------------------------

test_that(".face_parcel_ids resolves majority parcels per face", {
  # Vertex ids 1:4 -> parcels 1, 1, 2, 2
  faces <- rbind(
    c(1L, 2L, 3L),  # parcels 1,1,2 -> majority 1
    c(1L, 3L, 4L),  # parcels 1,2,2 -> majority 2
    c(1L, 2L, 4L)   # parcels 1,1,2 -> majority 1
  )
  parcel_ids <- c(1L, 1L, 2L, 2L)
  expect_equal(.face_parcel_ids(parcel_ids, faces), c(1L, 2L, 1L))
})


# ---- .compute_boundary_edges ------------------------------------------------

test_that(".compute_boundary_edges returns NULL on an empty mesh", {
  expect_null(.compute_boundary_edges(
    vis_faces = matrix(integer(), ncol = 3),
    vis_parcel = integer(),
    proj_xy = matrix(numeric(), ncol = 2)
  ))
})

test_that(".compute_boundary_edges flags silhouette edges on single-parcel mesh", {
  sq <- .square_mesh()
  proj_xy <- sq$verts[, 2:3]  # project onto (y, z)
  edges <- .compute_boundary_edges(
    vis_faces = sq$faces,
    vis_parcel = c(1L, 1L),
    proj_xy = proj_xy
  )
  expect_true(!is.null(edges))
  # Shared interior edge is paired + same-parcel -> filtered out; outer ring
  # is 4 unpaired silhouette edges.
  expect_true(all(edges$edge_type == "silhouette"))
  expect_equal(nrow(edges), 4L)
})

test_that(".compute_boundary_edges flags parcel boundaries and networks", {
  # Two triangles sharing edge (1,3), but different parcels.
  sq <- .square_mesh()
  proj_xy <- sq$verts[, 2:3]
  edges <- .compute_boundary_edges(
    vis_faces = sq$faces,
    vis_parcel = c(1L, 2L),
    proj_xy = proj_xy,
    id_to_network = c(`1` = "DMN", `2` = "Vis")
  )
  expect_true("parcel" %in% edges$edge_type ||
              "network" %in% edges$edge_type)
  # Exactly one shared-edge boundary between the two triangles.
  shared <- edges[edges$edge_type %in% c("parcel", "network"), , drop = FALSE]
  expect_equal(nrow(shared), 1L)
  expect_equal(unique(shared$edge_type), "network")
})


# ---- .chaikin_smooth_xy -----------------------------------------------------

test_that(".chaikin_smooth_xy is identity when n_iter = 0", {
  res <- .chaikin_smooth_xy(x = c(0, 1, 2), y = c(0, 1, 0), n_iter = 0L)
  expect_equal(res$x, c(0, 1, 2))
  expect_equal(res$y, c(0, 1, 0))
})

test_that(".chaikin_smooth_xy grows open polyline point count per iter", {
  x <- c(0, 1, 2, 3)
  y <- c(0, 1, 1, 0)
  n0 <- length(x)
  res1 <- .chaikin_smooth_xy(x, y, n_iter = 1L)
  # Open polyline: endpoints preserved, each interior edge contributes 2
  # cut-points, giving 2*n points per iteration in this implementation.
  expect_equal(length(res1$x), 2L * n0)
  expect_equal(length(res1$y), 2L * n0)
  # Endpoints preserved.
  expect_equal(res1$x[1], x[1])
  expect_equal(res1$y[1], y[1])
  expect_equal(res1$x[length(res1$x)], x[length(x)])
  expect_equal(res1$y[length(res1$y)], y[length(y)])
})

test_that(".chaikin_smooth_xy preserves closure for closed polylines", {
  # Closed quad (first == last)
  x <- c(0, 1, 1, 0, 0)
  y <- c(0, 0, 1, 1, 0)
  res <- .chaikin_smooth_xy(x, y, n_iter = 2L)
  n <- length(res$x)
  expect_equal(res$x[1], res$x[n])
  expect_equal(res$y[1], res$y[n])
})


# ---- .mesh_vertex_neighbors -------------------------------------------------

test_that(".mesh_vertex_neighbors returns correct adjacency for a square mesh", {
  sq <- .square_mesh()
  nb <- .mesh_vertex_neighbors(sq$faces, n_vertices = 4L)
  expect_equal(length(nb), 4L)
  # Each corner must see its two edge-mates; vertices 1 and 3 are connected via
  # the shared diagonal.
  expect_setequal(nb[[1]], c(2L, 3L, 4L))
  expect_setequal(nb[[3]], c(1L, 2L, 4L))
})

test_that(".mesh_vertex_neighbors handles empty mesh gracefully", {
  nb <- .mesh_vertex_neighbors(matrix(integer(), ncol = 3), n_vertices = 0L)
  expect_equal(length(nb), 0L)
})


# ---- .smooth_projected_xy ---------------------------------------------------

test_that(".smooth_projected_xy is a no-op when n_iter <= 0", {
  xy <- matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2, byrow = TRUE)
  nb <- list(2:3, c(1L, 3L), 1:2)
  out <- .smooth_projected_xy(xy, nb, n_iter = 0L, lambda = 0.5)
  expect_identical(out, xy)
})

test_that(".smooth_projected_xy pulls interior vertex toward neighbour centroid", {
  # Triangle with one "interior" vertex at (10, 10) whose two neighbours are
  # the other corners at (0, 0) and (2, 0). Smoothing should drag (10, 10)
  # toward the centroid (1, 0).
  xy <- matrix(c(0, 0, 2, 0, 10, 10), ncol = 2, byrow = TRUE)
  nb <- list(2:3, c(1L, 3L), 1:2)
  out <- .smooth_projected_xy(xy, nb, n_iter = 1L, lambda = 0.5)
  expect_true(out[3, 1] < xy[3, 1])
  expect_true(out[3, 2] < xy[3, 2])
})


# ---- project_surface_view (exported) ----------------------------------------

test_that("project_surface_view returns xy + view_dir for known views", {
  verts <- rbind(c(1, 2, 3), c(-1, 0, 4))

  lat_left <- project_surface_view(verts, view = "lateral", hemi = "left")
  expect_equal(lat_left$view_dir, c(-1, 0, 0))
  expect_equal(lat_left$xy, cbind(verts[, 2], verts[, 3]))

  dor_right <- project_surface_view(verts, view = "dorsal", hemi = "right")
  expect_equal(dor_right$view_dir, c(0, 0, 1))
  expect_equal(dor_right$xy, cbind(-verts[, 1], verts[, 2]))
})

test_that("project_surface_view rejects non-3-column inputs", {
  expect_error(
    project_surface_view(matrix(1:4, ncol = 2)),
    "must be a numeric matrix with 3 columns"
  )
})


# ---- .encode_plot_brain_data_id --------------------------------------------

test_that(".encode_plot_brain_data_id produces stable :: joined ids", {
  ids <- .encode_plot_brain_data_id(
    panel = c("Left Lateral", "Right Medial"),
    parcel_id = c(3, 17),
    shape_id = c(1, 2)
  )
  expect_equal(ids, c("Left Lateral::3::1", "Right Medial::17::2"))
})
