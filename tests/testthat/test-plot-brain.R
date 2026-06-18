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

test_that("project_surface_view validates and projects vertices", {
  verts <- matrix(c(
    -1, 0, 0,
    -1, 1, 0,
    -1, 0, 1
  ), ncol = 3, byrow = TRUE)

  proj <- project_surface_view(verts, view = "lateral", hemi = "left")
  expect_true(is.list(proj))
  expect_true(all(c("xy", "view_dir") %in% names(proj)))
  expect_equal(dim(proj$xy), c(3, 2))
  expect_equal(length(proj$view_dir), 3)
})

test_that("build_surface_polygon_data rejects non-surfatlas input", {
  fake <- list(name = "fake")
  class(fake) <- "atlas"
  expect_error(build_surface_polygon_data(fake), "surfatlas")
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

test_that(".face_depth computes mean depth along a view direction", {
  verts <- matrix(c(
    0, 0, 1,
    0, 0, 2,
    0, 0, 3,
    0, 0, 6,
    0, 0, 7,
    0, 0, 8
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(
    1L, 2L, 3L,
    4L, 5L, 6L
  ), ncol = 3, byrow = TRUE)

  depth <- neuroatlas:::.face_depth(verts, faces, c(0, 0, 1))

  expect_equal(depth, c(2, 7))
})

test_that(".depth_cull_faces removes far faces at the same projected location", {
  faces <- matrix(c(
    1L, 2L, 3L,
    4L, 5L, 6L,
    7L, 8L, 9L
  ), ncol = 3, byrow = TRUE)
  proj_xy <- matrix(c(
    0, 0,
    1, 0,
    0, 1,
    0, 0,
    1, 0,
    0, 1,
    5, 5,
    6, 5,
    5, 6
  ), ncol = 2, byrow = TRUE)
  face_depth <- c(10, 1, 0)

  kept <- neuroatlas:::.depth_cull_faces(
    faces,
    proj_xy,
    face_depth,
    candidates = seq_len(nrow(faces)),
    resolution = 32L,
    tolerance = 0,
    neighborhood = 0L
  )

  expect_equal(kept, c(1L, 3L))
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
  expect_true(all(c("v1", "v2") %in% names(edges)))

  # Outer square edges appear once -> silhouette (4 edges)
  expect_equal(sum(edges$edge_type == "silhouette"), 4L)
  # Shared diagonal is between different parcels and networks -> network (1 edge)
  expect_equal(sum(edges$edge_type == "network"), 1L)
})

test_that(".boundary_edges_to_paths chains undirected edges into ordered paths", {
  edges <- tibble::tibble(
    panel = "Left Lateral",
    edge_type = "parcel",
    v1 = c(1L, 2L, 3L, 4L),
    v2 = c(2L, 3L, 4L, 1L),
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1),
    xend = c(1, 1, 0, 0),
    yend = c(0, 1, 1, 0)
  )

  paths <- neuroatlas:::.boundary_edges_to_paths(edges)
  expect_s3_class(paths, "tbl_df")
  expect_true(all(c("x", "y", "path_id", "vertex_order", "edge_type", "panel") %in%
                    names(paths)))

  # A simple 4-edge cycle should become one path
  expect_equal(length(unique(paths$path_id)), 1L)

  got <- unique(paste0(paths$x, ",", paths$y))
  expect_setequal(got, c("0,0", "1,0", "1,1", "0,1"))
})

test_that(".smooth_boundary_paths increases path density while preserving ids", {
  edges <- tibble::tibble(
    panel = "Left Lateral",
    edge_type = "parcel",
    v1 = c(1L, 2L, 3L, 4L),
    v2 = c(2L, 3L, 4L, 1L),
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1),
    xend = c(1, 1, 0, 0),
    yend = c(0, 1, 1, 0)
  )

  paths <- neuroatlas:::.boundary_edges_to_paths(edges)
  smooth0 <- neuroatlas:::.smooth_boundary_paths(paths, n_iter = 0L)
  smooth1 <- neuroatlas:::.smooth_boundary_paths(paths, n_iter = 1L)

  expect_equal(smooth0$x, paths$x)
  expect_equal(smooth0$y, paths$y)
  expect_gt(nrow(smooth1), nrow(paths))
  expect_equal(unique(smooth1$path_id), unique(paths$path_id))
  expect_equal(unique(smooth1$edge_type), unique(paths$edge_type))
  expect_equal(unique(smooth1$panel), unique(paths$panel))
})

test_that(".mesh_vertex_neighbors builds expected adjacency from faces", {
  faces <- matrix(
    c(1L, 2L, 3L,
      1L, 3L, 4L),
    ncol = 3,
    byrow = TRUE
  )

  nb <- neuroatlas:::.mesh_vertex_neighbors(faces, n_vertices = 4L)

  expect_length(nb, 4L)
  expect_setequal(nb[[1]], c(2L, 3L, 4L))
  expect_setequal(nb[[2]], c(1L, 3L))
  expect_setequal(nb[[3]], c(1L, 2L, 4L))
  expect_setequal(nb[[4]], c(1L, 3L))
})

test_that(".smooth_projected_xy smooths coordinates while preserving shape", {
  faces <- matrix(
    c(1L, 2L, 3L,
      1L, 3L, 4L),
    ncol = 3,
    byrow = TRUE
  )
  nb <- neuroatlas:::.mesh_vertex_neighbors(faces, n_vertices = 4L)

  xy <- matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1),
    ncol = 2,
    byrow = TRUE
  )

  sm0 <- neuroatlas:::.smooth_projected_xy(xy, nb, n_iter = 0L)
  sm1 <- neuroatlas:::.smooth_projected_xy(xy, nb, n_iter = 1L, lambda = 0.5)

  expect_equal(sm0, xy)
  expect_equal(dim(sm1), dim(xy))
  expect_true(any(abs(sm1 - xy) > 1e-8))
})

test_that(".encode_plot_brain_data_id creates stable polygon keys", {
  key <- neuroatlas:::.encode_plot_brain_data_id(
    panel = "Left Lateral",
    parcel_id = 12L,
    shape_id = 44L
  )
  expect_equal(key, "Left Lateral::12::44")
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

  atl <- tryCatch({
    schaefer_surf(100, 7)
  }, error = function(e) {
    skip(paste("Surface atlas unavailable:", conditionMessage(e)))
  })

  p <- plot_brain(atl, interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_brain validates the 'background' argument before any rendering", {
  # Runs offline: the check fires before the surfatlas/network path.
  expect_error(plot_brain(NULL, background = "yes"),
               "'background' must be TRUE or FALSE")
  expect_error(plot_brain(NULL, background = NA),
               "'background' must be TRUE or FALSE")
  expect_error(plot_brain(NULL, depth_cull = NA),
               "'depth_cull' must be TRUE or FALSE")
})

test_that("plot_brain draws a cortex backdrop when background = TRUE", {
  skip_on_cran()

  atl <- tryCatch({
    schaefer_surf(100, 7)
  }, error = function(e) {
    skip(paste("Surface atlas unavailable:", conditionMessage(e)))
  })

  p_bg <- plot_brain(atl, views = "lateral", interactive = FALSE,
                     background = TRUE)
  p_no <- plot_brain(atl, views = "lateral", interactive = FALSE,
                     background = FALSE)
  expect_s3_class(p_bg, "ggplot")
  # The backdrop adds one extra geom_polygon layer beneath the parcels.
  expect_gt(length(p_bg$layers), length(p_no$layers))

  # The silhouette builder yields one or more filled polygons per panel.
  sil <- neuroatlas:::.build_surface_silhouette_data(
    atl, views = "lateral", hemis = c("left", "right"),
    surface = "inflated", projection_smooth = 0L
  )
  expect_true(is.data.frame(sil) && nrow(sil) > 0)
  expect_true(all(c("x", "y", "poly_id", "panel") %in% names(sil)))
})

test_that("plot_brain normalizes colorbar position inputs", {
  expect_equal(neuroatlas:::.normalize_colorbar_position(TRUE), "right")
  expect_equal(neuroatlas:::.normalize_colorbar_position(FALSE), "none")
  expect_equal(neuroatlas:::.normalize_colorbar_position("bottom"), "bottom")
  expect_error(
    neuroatlas:::.normalize_colorbar_position("left"),
    "'colorbar' must be TRUE, FALSE"
  )
})

test_that("plot_brain applies static annotations and panel label overrides", {
  skip_on_cran()

  atl <- tryCatch({
    schaefer_surf(100, 7)
  }, error = function(e) {
    skip(paste("Surface atlas unavailable:", conditionMessage(e)))
  })

  p <- plot_brain(
    atl,
    views = "lateral",
    interactive = FALSE,
    title = "Surface map",
    panel_labels = c(
      "Left Lateral" = "LH lateral",
      "Right Lateral" = "RH lateral"
    )
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Surface map")

  strip_labels <- p$facet$params$labeller(data.frame(
    panel = c("Left Lateral", "Right Lateral")
  ))
  expect_equal(unname(strip_labels$panel), c("LH lateral", "RH lateral"))
})

test_that("plot_brain composes a bottom colorbar when requested", {
  skip_on_cran()
  skip_if_not_installed("patchwork")

  atl <- tryCatch({
    schaefer_surf(100, 7)
  }, error = function(e) {
    skip(paste("Surface atlas unavailable:", conditionMessage(e)))
  })

  p <- plot_brain(
    atl,
    vals = rnorm(length(atl$ids)),
    views = "lateral",
    interactive = FALSE,
    colorbar = "bottom",
    colorbar_title = "z-score",
    title = "Contrast"
  )

  expect_s3_class(p, "patchwork")
  expect_s3_class(patchwork::patchworkGrob(p), "gtable")
})

test_that(".repair_legacy_surface_geometry rebuilds bundled legacy fsaverage", {
  skip_if_not_installed("neurosurf")
  e <- new.env()
  utils::data("fsaverage", package = "neuroatlas", envir = e)
  fsavg <- e$fsaverage
  skip_if_not(length(fsavg) > 0, "bundled fsaverage data unavailable")
  legacy <- fsavg[[1]]
  # Sanity: bundled data is missing slots added in current SurfaceGeometry.
  expect_error(methods::validObject(legacy))

  repaired <- neuroatlas:::.repair_legacy_surface_geometry(legacy)
  expect_silent(methods::validObject(repaired))
  expect_true(is.matrix(neurosurf::surf_to_world(repaired)))
  expect_equal(nrow(t(repaired@mesh$vb[1:3, , drop = FALSE])),
               nrow(t(legacy@mesh$vb[1:3, , drop = FALSE])))
})

test_that(".resolve_overlay_surface_pair returns geometries with all slots", {
  skip_on_cran()
  skip_if_not_installed("neurosurf")
  skip_if_not_installed("neuroatlas")
  atl <- tryCatch(
    neuroatlas::schaefer_surf(200, 7, space = "fsaverage6", surf = "inflated"),
    error = function(e) skip(paste("schaefer_surf unavailable:", conditionMessage(e)))
  )

  pair <- neuroatlas:::.resolve_overlay_surface_pair(atl, hemi = "lh")
  expect_silent(methods::validObject(pair$white))
  expect_silent(methods::validObject(pair$pial))
  expect_true(is.matrix(neurosurf::surf_to_world(pair$white)))
  expect_true(is.matrix(neurosurf::surf_to_world(pair$pial)))
})

test_that("volume->surface overlay leaves off-coverage vertices NA (transparent)", {
  skip_if_not_installed("neurosurf")
  skip_if_not_installed("neuroim2")

  sp <- neuroim2::NeuroSpace(c(10L, 10L, 10L), spacing = c(1, 1, 1))
  arr <- array(0, dim = c(10, 10, 10))
  arr[4:6, 4:6, 4:6] <- 1.5
  vol <- neuroim2::NeuroVol(arr, space = sp)

  verts <- matrix(c(
    4, 4, 4,
    50, 50, 50,
    4, 5, 4,
    4, 6, 4
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 2L, 3L, 1L, 2L, 3L), ncol = 3, byrow = TRUE)

  surf_wm <- neurosurf::SurfaceGeometry(vert = verts,
                                        faces = faces, hemi = "left")
  surf_pial <- neurosurf::SurfaceGeometry(vert = verts + 0.1,
                                          faces = faces, hemi = "left")

  vals <- neuroatlas:::.project_overlay_one_hemi(
    cluster_vol = vol,
    surf_wm = surf_wm, surf_pial = surf_pial,
    target_n = nrow(verts), fun = "avg", sampling = "midpoint"
  )

  expect_true(is.finite(vals[1]))
  expect_true(is.na(vals[2]))
  expect_equal(vals[1], 1.5, tolerance = 1e-6)
})
