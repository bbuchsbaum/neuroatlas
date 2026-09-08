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
