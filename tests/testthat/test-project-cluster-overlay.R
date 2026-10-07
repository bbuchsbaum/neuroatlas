# A two-hemisphere toy surface atlas built on its own white mesh, so the
# projection uses that mesh instead of loading template surfaces.
.make_projection_test_atlas <- function() {
  skip_if_not_installed("neurosurf")
  verts <- matrix(
    c(
      4,
      4,
      4,
      50,
      50,
      50,
      4,
      5,
      4,
      4,
      6,
      4
    ),
    ncol = 3,
    byrow = TRUE
  )
  faces <- matrix(c(0L, 2L, 3L, 1L, 2L, 3L), ncol = 3, byrow = TRUE)
  hemi_surf <- function(hemi) {
    geom <- neurosurf::SurfaceGeometry(
      vert = verts,
      faces = faces,
      hemi = hemi
    )
    methods::new(
      "LabeledNeuroSurface",
      labels = "cortex",
      cols = "#888888",
      geometry = geom,
      indices = seq_len(nrow(verts)),
      data = rep(1, nrow(verts))
    )
  }
  structure(
    list(
      name = "projection-test",
      ids = 1L,
      labels = "toy",
      orig_labels = "toy",
      hemi = "left",
      cmap = data.frame(r = 180, g = 180, b = 180),
      lh_atlas = hemi_surf("lh"),
      rh_atlas = hemi_surf("rh"),
      surf_type = "white",
      surface_space = "toy"
    ),
    class = c("projection_test", "surfatlas", "atlas")
  )
}

.make_projection_test_volume <- function() {
  sp <- neuroim2::NeuroSpace(c(10L, 10L, 10L), spacing = c(1, 1, 1))
  arr <- array(0, dim = c(10, 10, 10))
  arr[4:6, 4:6, 4:6] <- 1.5
  neuroim2::NeuroVol(arr, space = sp)
}

test_that(
  "project_cluster_overlay returns one value per atlas vertex",
  {
    atl <- .make_projection_test_atlas()
    vol <- .make_projection_test_volume()
    # No pial template exists for the toy space: fall back to the atlas mesh.
    local_mocked_bindings(.load_overlay_surface_geometry = function(...) NULL)

    proj <- project_cluster_overlay(vol, atl)

    expect_named(proj, c("overlay", "meta"))
    expect_named(proj$overlay, c("lh", "rh"))
    for (h in c("lh", "rh")) {
      v <- proj$overlay[[h]]
      expect_type(v, "double")
      expect_length(v, length(atl[[paste0(h, "_atlas")]]@data))
      expect_null(attributes(v))
      expect_equal(v[1], 1.5, tolerance = 1e-6)
      # A vertex outside the volume is not reached by the projection.
      expect_true(is.na(v[2]))
      expect_equal(proj$meta$hemis[[h]]$finite_vertices, sum(is.finite(v)))
      expect_null(proj$meta$hemis[[h]]$error)
    }
    expect_identical(proj$meta$surface_space, "toy")
    expect_identical(proj$meta$hemis$lh$sampling, "midpoint")
  }
)

test_that(
  "project_cluster_overlay matches the projection plot_brain draws",
  {
    atl <- .make_projection_test_atlas()
    vol <- .make_projection_test_volume()
    local_mocked_bindings(.load_overlay_surface_geometry = function(...) NULL)

    public <- project_cluster_overlay(
      vol,
      atl,
      fun = "nn",
      interpolation = "nearest"
    )
    internal <- .project_cluster_overlay(
      vol,
      atl,
      fun = "nn",
      interpolation = "nearest"
    )
    expect_identical(public, internal)

    direct <- neurosurf::vol_to_surf(
      surf_wm = atl$lh_atlas@geometry,
      surf_pial = atl$lh_atlas@geometry,
      vol = vol,
      fun = "nn",
      interpolation = "nearest",
      fill = NA_real_
    )
    expect_equal(public$overlay$lh, as.numeric(neurosurf::values(direct)))
  }
)

test_that(
  "project_cluster_overlay validates its inputs",
  {
    atl <- .make_projection_test_atlas()
    vol <- .make_projection_test_volume()

    expect_error(
      project_cluster_overlay(array(0, c(2, 2, 2)), atl),
      "NeuroVol"
    )
    expect_error(
      project_cluster_overlay(vol, list(lh_atlas = NULL)),
      "surfatlas"
    )
    empty <- structure(list(name = "empty"), class = c("surfatlas", "atlas"))
    expect_error(project_cluster_overlay(vol, empty), "neither an lh nor an rh")
    expect_error(project_cluster_overlay(vol, atl, fun = "median"))
    expect_error(
      project_cluster_overlay(vol, atl, surface_smooth_fwhm = -1),
      "non-negative"
    )
  }
)

test_that(
  "project_cluster_overlay warns when a hemisphere cannot be projected",
  {
    atl <- .make_projection_test_atlas()
    vol <- .make_projection_test_volume()
    local_mocked_bindings(
      .resolve_overlay_surface_pair = function(surfatlas, hemi, ...) {
        geom <- surfatlas[[paste0(hemi, "_atlas")]]@geometry
        if (identical(hemi, "rh")) geom <- NULL
        list(white = geom, pial = geom, surface_space = "toy")
      }
    )

    expect_warning(
      proj <- project_cluster_overlay(vol, atl),
      "rh hemisphere failed"
    )
    expect_true(all(is.na(proj$overlay$rh)))
    expect_length(proj$overlay$rh, 4L)
    expect_null(attributes(proj$overlay$rh))
    expect_type(proj$meta$hemis$rh$error, "character")
    expect_equal(proj$overlay$lh[1], 1.5, tolerance = 1e-6)

    # The internal used by plot_brain() keeps failing silently.
    expect_no_warning(.project_cluster_overlay(vol, atl))
  }
)
