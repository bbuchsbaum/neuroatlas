test_that(".face_normals computes correct normals", {
  # Simple triangle in the xy-plane: vertices at (0,0,0), (1,0,0), (0,1,0)
  verts <- matrix(c(0, 0, 0,
                     1, 0, 0,
                     0, 1, 0), nrow = 3, byrow = TRUE)
  faces <- matrix(c(1, 2, 3), nrow = 1)

  normals <- neuroatlas:::.face_normals(verts, faces)

  expect_equal(nrow(normals), 1)
  expect_equal(ncol(normals), 3)
  # Normal should point in the +z direction
  expect_equal(normals[1, 3], 1, tolerance = 1e-10)
  expect_equal(normals[1, 1], 0, tolerance = 1e-10)
  expect_equal(normals[1, 2], 0, tolerance = 1e-10)
})

test_that(".face_normals handles multiple faces", {
  verts <- matrix(c(0, 0, 0,
                     1, 0, 0,
                     0, 1, 0,
                     0, 0, 1), nrow = 4, byrow = TRUE)
  faces <- matrix(c(1, 2, 3,
                     1, 2, 4), nrow = 2, byrow = TRUE)

  normals <- neuroatlas:::.face_normals(verts, faces)

  expect_equal(nrow(normals), 2)
  # First face normal: +z

  expect_equal(normals[1, 3], 1, tolerance = 1e-10)
  # Second face normal: should be perpendicular to edge (1,0,0) and (0,0,1)
  # Cross product of (1,0,0) x (0,0,1) = (0,-1,0), normalised = (0,-1,0)
  expect_equal(abs(normals[2, 2]), 1, tolerance = 1e-10)
})

test_that(".face_parcel_ids assigns by majority vote", {
  parcel_ids <- c(1L, 1L, 2L, 2L, 3L)
  faces <- matrix(c(1, 2, 3,   # parcels: 1, 1, 2 -> majority = 1
                     3, 4, 5,   # parcels: 2, 2, 3 -> majority = 2
                     1, 3, 5),  # parcels: 1, 2, 3 -> no majority, returns 1
                  nrow = 3, byrow = TRUE)

  result <- neuroatlas:::.face_parcel_ids(parcel_ids, faces)

  expect_equal(length(result), 3)
  expect_equal(result[1], 1L)
  expect_equal(result[2], 2L)
  # For all-different case, the function returns p1 (first vertex)
  expect_true(result[3] %in% c(1L, 2L, 3L))
})

test_that(".merge_parcel_polygons is robust to inconsistent face winding", {
  # Square split into two triangles, with the second triangle deliberately
  # reversed (inconsistent winding). Boundary detection should be
  # orientation-invariant and still recover the outer square polygon.
  proj_xy <- matrix(c(
    0, 0,  # 1
    1, 0,  # 2
    1, 1,  # 3
    0, 1   # 4
  ), ncol = 2, byrow = TRUE)

  vis_faces <- matrix(c(
    1, 2, 3,
    1, 4, 3
  ), ncol = 3, byrow = TRUE)

  vis_parcel <- c(1L, 1L)
  id_to_label <- c("1" = "square")

  poly <- neuroatlas:::.merge_parcel_polygons(
    vis_faces,
    vis_parcel,
    proj_xy,
    id_to_label
  )

  expect_s3_class(poly, "tbl_df")
  expect_true(all(c("x", "y", "poly_id", "parcel_id", "label") %in% names(poly)))

  expect_equal(length(unique(poly$poly_id)), 1L)
  expect_equal(unique(poly$parcel_id), 1L)

  got <- unique(paste0(poly$x, ",", poly$y))
  expected <- paste0(proj_xy[, 1], ",", proj_xy[, 2])
  expect_setequal(got, expected)
})

test_that(".compute_boundary_edges classifies silhouette and network boundaries", {
  proj_xy <- matrix(c(
    0, 0,  # 1
    1, 0,  # 2
    1, 1,  # 3
    0, 1   # 4
  ), ncol = 2, byrow = TRUE)

  # Two triangles sharing the (1,3) diagonal
  vis_faces <- matrix(c(
    1, 2, 3,
    1, 3, 4
  ), ncol = 3, byrow = TRUE)

  # Different parcels (and networks) on each face
  vis_parcel <- c(1L, 2L)
  id_to_network <- c("1" = "A", "2" = "B")

  edges <- neuroatlas:::.compute_boundary_edges(
    vis_faces,
    vis_parcel,
    proj_xy,
    id_to_network = id_to_network
  )

  expect_s3_class(edges, "tbl_df")
  expect_true("edge_type" %in% names(edges))

  # Outer square edges appear once -> silhouette (4 edges)
  expect_equal(sum(edges$edge_type == "silhouette"), 4L)
  # Shared diagonal is between different parcels and networks -> network (1 edge)
  expect_equal(sum(edges$edge_type == "network"), 1L)
})

test_that("plot_brain rejects non-surfatlas input", {
  fake <- list(name = "fake")
  class(fake) <- "atlas"

  expect_error(plot_brain(fake), "surfatlas")
})

test_that("plot_brain rejects vals of wrong length", {
  skip_on_cran()

  # Create a minimal surfatlas-like object for validation testing
  fake_surf <- list(ids = 1:10, labels = paste0("R", 1:10))
  class(fake_surf) <- c("schaefer", "surfatlas", "atlas")

  expect_error(plot_brain(fake_surf, vals = 1:5), "Length of 'vals'")
})

test_that("plot_brain returns ggplot when interactive = FALSE", {
  skip_on_cran()
  skip("Requires network access to download surface atlas")

  # This test would require a real surfatlas object
  # atl <- schaefer_surf(100, 7)
  # p <- plot_brain(atl, interactive = FALSE)
  # expect_s3_class(p, "ggplot")
})
