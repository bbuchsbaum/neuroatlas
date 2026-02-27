# tests/testthat/test-spin-test.R

# Build a synthetic sphere + surfatlas for testing without TemplateFlow.
# Creates two hemispheres with 5 parcels each on a unit sphere.
make_spin_atlas <- function() {
  # Generate sphere vertices: regular grid on unit sphere via spherical coords
  n_vert <- 200  # per hemisphere
  set.seed(123)
  theta <- stats::runif(n_vert, 0, pi)
  phi   <- stats::runif(n_vert, 0, 2 * pi)
  lh_coords <- cbind(
    sin(theta) * cos(phi),
    sin(theta) * sin(phi),
    cos(theta)
  )
  rh_coords <- cbind(
    -sin(theta) * cos(phi),  # mirror x for right hemi
    sin(theta) * sin(phi),
    cos(theta)
  )

  # Assign parcels based on z-coordinate bins (5 parcels per hemi)
  z_breaks <- seq(-1, 1, length.out = 6)
  lh_labels <- as.integer(cut(lh_coords[, 3], z_breaks, include.lowest = TRUE))
  rh_labels <- as.integer(cut(rh_coords[, 3], z_breaks, include.lowest = TRUE)) + 5L

  # Build minimal SurfaceGeometry + labeled surface-like objects
  # We use a simple structure that has @data for labels
  lh_atlas_obj <- structure(list(), class = "mock_surf")
  lh_atlas_obj@data <- lh_labels
  rh_atlas_obj <- structure(list(), class = "mock_surf")
  rh_atlas_obj@data <- rh_labels

  # Use setattr approach instead since mock objects need @data slot
  lh_atlas_obj <- new("mock_labeled_surf", data = lh_labels)
  rh_atlas_obj <- new("mock_labeled_surf", data = rh_labels)

  atlas <- structure(list(
    name     = "spin_test_atlas",
    ids      = 1:10,
    labels   = paste0("R", 1:10),
    orig_labels = paste0("Region_", 1:10),
    hemi     = rep(c("left", "right"), each = 5),
    lh_atlas = lh_atlas_obj,
    rh_atlas = rh_atlas_obj,
    surface_space = "fsaverage6"
  ), class = c("test_surfatlas", "surfatlas", "atlas"))

  sphere <- list(lh = lh_coords, rh = rh_coords)

  list(atlas = atlas, sphere = sphere)
}

# We need a simple S4 class with a @data slot for the mock
# Define it outside of tests so it's available
if (!isClass("mock_labeled_surf")) {
  setClass("mock_labeled_surf", representation(data = "integer"))
}

make_spin_atlas <- function() {
  n_vert <- 200
  set.seed(123)
  theta <- stats::runif(n_vert, 0, pi)
  phi   <- stats::runif(n_vert, 0, 2 * pi)
  lh_coords <- cbind(
    sin(theta) * cos(phi),
    sin(theta) * sin(phi),
    cos(theta)
  )
  rh_coords <- cbind(
    -sin(theta) * cos(phi),
    sin(theta) * sin(phi),
    cos(theta)
  )

  z_breaks <- seq(-1, 1, length.out = 6)
  lh_labels <- as.integer(cut(lh_coords[, 3], z_breaks, include.lowest = TRUE))
  rh_labels <- as.integer(cut(rh_coords[, 3], z_breaks, include.lowest = TRUE)) + 5L

  lh_atlas_obj <- new("mock_labeled_surf", data = lh_labels)
  rh_atlas_obj <- new("mock_labeled_surf", data = rh_labels)

  atlas <- structure(list(
    name        = "spin_test_atlas",
    ids         = 1:10,
    labels      = paste0("R", 1:10),
    orig_labels = paste0("Region_", 1:10),
    hemi        = rep(c("left", "right"), each = 5),
    lh_atlas    = lh_atlas_obj,
    rh_atlas    = rh_atlas_obj,
    surface_space = "fsaverage6"
  ), class = c("test_surfatlas", "surfatlas", "atlas"))

  sphere <- list(lh = lh_coords, rh = rh_coords)

  list(atlas = atlas, sphere = sphere)
}


test_that("spin_test returns correct structure", {
  skip_if_not_installed("Rnanoflann")
  sa <- make_spin_atlas()
  K  <- length(sa$atlas$ids)

  res <- spin_test(
    map1 = rnorm(K), map2 = rnorm(K),
    atlas = sa$atlas, sphere = sa$sphere,
    n_perm = 50, seed = 1
  )

  expect_s3_class(res, "spin_test")
  expect_true(is.numeric(res$observed))
  expect_length(res$null_distribution, 50L)
  expect_true(res$p_value >= 0 && res$p_value <= 1)
  expect_equal(res$n_perm, 50L)
})


test_that("identical maps produce low p-value", {
  skip_if_not_installed("Rnanoflann")
  sa <- make_spin_atlas()
  K  <- length(sa$atlas$ids)

  set.seed(42)
  map <- rnorm(K)

  res <- spin_test(
    map1 = map, map2 = map,
    atlas = sa$atlas, sphere = sa$sphere,
    n_perm = 199, seed = 99
  )

  # With identical maps, observed r = 1.0
  expect_equal(res$observed, 1.0)
  # p-value should be small (most null correlations < 1.0)
  expect_true(res$p_value < 0.2)
})


test_that("uncorrelated maps produce non-significant p-value", {
  skip_if_not_installed("Rnanoflann")
  sa <- make_spin_atlas()
  K  <- length(sa$atlas$ids)

  set.seed(7)
  map1 <- rnorm(K)
  map2 <- rnorm(K)

  res <- spin_test(
    map1 = map1, map2 = map2,
    atlas = sa$atlas, sphere = sa$sphere,
    n_perm = 199, seed = 7
  )

  # With random maps, p should generally be non-significant
  # Use a generous threshold since K=10 is small
  expect_true(res$p_value > 0.01)
})


test_that("seed produces reproducible results", {
  skip_if_not_installed("Rnanoflann")
  sa <- make_spin_atlas()
  K  <- length(sa$atlas$ids)
  m1 <- seq_len(K) * 1.0
  m2 <- rev(seq_len(K)) * 1.0

  r1 <- spin_test(m1, m2, sa$atlas, sphere = sa$sphere, n_perm = 50, seed = 42)
  r2 <- spin_test(m1, m2, sa$atlas, sphere = sa$sphere, n_perm = 50, seed = 42)

  expect_equal(r1$null_distribution, r2$null_distribution)
  expect_equal(r1$p_value, r2$p_value)
})


test_that("input validation catches wrong lengths", {
  skip_if_not_installed("Rnanoflann")
  sa <- make_spin_atlas()

  expect_error(
    spin_test(1:5, 1:10, sa$atlas, sphere = sa$sphere, n_perm = 10),
    "length\\(map1\\)"
  )
  expect_error(
    spin_test(1:10, 1:5, sa$atlas, sphere = sa$sphere, n_perm = 10),
    "length\\(map2\\)"
  )
})


test_that(".random_rotation_matrix returns proper rotation", {
  set.seed(1)
  Q <- neuroatlas:::.random_rotation_matrix()

  expect_equal(dim(Q), c(3L, 3L))
  # Orthogonal: Q^T Q = I
  expect_equal(t(Q) %*% Q, diag(3), tolerance = 1e-10)
  # Proper rotation: det = +1
  expect_equal(det(Q), 1.0, tolerance = 1e-10)
})


test_that("print.spin_test works", {
  skip_if_not_installed("Rnanoflann")
  sa <- make_spin_atlas()
  K  <- length(sa$atlas$ids)

  res <- spin_test(rnorm(K), rnorm(K), sa$atlas,
                   sphere = sa$sphere, n_perm = 10, seed = 1)

  out <- capture.output(print(res))
  expect_true(any(grepl("Spin Test", out)))
  expect_true(any(grepl("Observed r", out)))
  expect_true(any(grepl("p-value", out)))
})
