test_that("explicit anatomy is validated and retains its source", {
  geometry <- neurosurf::SurfaceGeometry(
    matrix(c(0,0,0, 0,2,0, 0,0,2), 3, 3, byrow=TRUE),
    matrix(c(0L,1L,2L), 1, 3), hemi="left")
  atlas <- structure(list(lh_atlas = methods::new("LabeledNeuroSurface",
    geometry=geometry, data=c(1,1,1), indices=1:3,
    labels="cortex", cols="#888888"), surf_type="inflated"), class="surfatlas")
  resolved <- surface_anatomy(atlas, "left", c(-1,0,1), "native_curv")
  expect_equal(resolved$metric, c(-1,0,1))
  expect_equal(surface_anatomy(atlas, metric=c(-1,0,1))$metric, resolved$metric)
  expect_identical(resolved$provenance$source, "native_curv")
  expect_error(surface_anatomy(atlas, "lh", c(1,2)), "one finite")
  expect_error(surface_anatomy(atlas, "lh", c(1,NA,2)), "one finite")
})

test_that("default fsaverage6 anatomy uses the packaged white mesh", {
  skip_on_cran()
  atlas <- get_schaefer_surfatlas(parcels = 100, networks = 7,
                                  surf = "inflated")
  expect_identical(atlas$density, "41k")
  resolved <- surface_anatomy(atlas, "lh")
  expect_identical(resolved$provenance$source, "computed_mean_curvature")
  expect_true(resolved$provenance$topology_verified)
  expect_gt(stats::sd(resolved$metric), 0)
})

test_that("sulcal-depth anatomy is computed from white vs inflated geometry", {
  skip_on_cran()
  atlas <- get_schaefer_surfatlas(parcels = 100, networks = 7,
                                  surf = "inflated")
  depth <- surface_anatomy(atlas, "lh", type = "sulcal_depth")
  curvature <- surface_anatomy(atlas, "lh")
  expect_identical(depth$provenance$source, "computed_sulcal_depth_proxy")
  expect_identical(depth$provenance$source_surface, "white")
  expect_true(depth$provenance$topology_verified)
  expect_length(depth$metric, length(atlas$lh_atlas@data))
  expect_true(all(is.finite(depth$metric)))
  # Sulcal depth is a smooth, large-scale field: it spans millimetres and
  # both signs, unlike the fine-grained curvature it replaces.
  expect_gt(diff(range(depth$metric)), 10)
  expect_true(any(depth$metric < 0) && any(depth$metric > 0))
  expect_false(isTRUE(all.equal(depth$metric, curvature$metric)))
  expect_identical(
    depth$provenance$mesh_identity, curvature$provenance$mesh_identity
  )
})

test_that("explicit or atlas metrics win over computed sulcal depth", {
  geometry <- neurosurf::SurfaceGeometry(
    matrix(c(0,0,0, 0,2,0, 0,0,2), 3, 3, byrow=TRUE),
    matrix(c(0L,1L,2L), 1, 3), hemi="left")
  atlas <- structure(list(lh_atlas = methods::new("LabeledNeuroSurface",
    geometry=geometry, data=c(1,1,1), indices=1:3,
    labels="cortex", cols="#888888"), surf_type="inflated"), class="surfatlas")
  explicit <- surface_anatomy(atlas, "lh", c(-1, 0, 1), "native_sulc",
                              type = "sulcal_depth")
  expect_equal(explicit$metric, c(-1, 0, 1))
  expect_identical(explicit$provenance$source, "native_sulc")
  expect_error(surface_anatomy(atlas, "lh", type = "depth"), "should be one of")
})

test_that("sulcal depth falls back to curvature on a white display surface", {
  atlas <- structure(list(surf_type = "white"), class = "surfatlas")
  expect_null(neuroatlas:::.resolve_sulcal_depth_anatomy(atlas, "lh"))
})
