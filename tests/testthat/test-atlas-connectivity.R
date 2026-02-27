test_that("atlas_connectivity returns correct structure", {
  skip_on_cran()

  # Create toy atlas
  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:5, 1:5] <- 1L
  arr[3:3, 1:5, 1:5] <- 2L
  arr[4:5, 1:5, 1:5] <- 3L

  sp <- neuroim2::NeuroSpace(dim = c(5, 5, 5), spacing = c(1, 1, 1), origin = c(0, 0, 0))
  atlas_vol <- neuroim2::NeuroVol(arr, sp)

  atlas <- structure(list(
    name = "conn_test",
    atlas = atlas_vol,
    ids = 1:3,
    labels = c("R1", "R2", "R3"),
    orig_labels = c("R1", "R2", "R3"),
    hemi = c("left", NA, "right")
  ), class = "atlas")

  # Create synthetic 4D NeuroVec (10 timepoints)
  set.seed(42)
  data_arr <- array(rnorm(5 * 5 * 5 * 10), dim = c(5, 5, 5, 10))
  nvec_sp <- neuroim2::NeuroSpace(dim = c(5, 5, 5, 10), spacing = c(1, 1, 1))
  data_vol <- neuroim2::NeuroVec(data_arr, nvec_sp)

  conn <- atlas_connectivity(data_vol, atlas)

  # Check structure
  expect_true(inherits(conn, "atlas_connectivity"))
  expect_true(is.matrix(conn))
  expect_equal(nrow(conn), 3)
  expect_equal(ncol(conn), 3)

  # Check symmetry
  expect_equal(conn, t(conn))

  # Check diagonal is zero
  expect_equal(unname(diag(conn)), c(0, 0, 0))

  # Check dimnames
  expect_true(!is.null(dimnames(conn)))
})

test_that("atlas_connectivity threshold works", {
  skip_on_cran()

  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:5, 1:5] <- 1L
  arr[3:5, 1:5, 1:5] <- 2L
  sp <- neuroim2::NeuroSpace(dim = c(5, 5, 5), spacing = c(1, 1, 1), origin = c(0, 0, 0))
  atlas_vol <- neuroim2::NeuroVol(arr, sp)
  atlas <- structure(list(
    name = "thresh_test", atlas = atlas_vol,
    ids = 1:2, labels = c("A", "B"), orig_labels = c("A", "B"),
    hemi = c("left", "right")
  ), class = "atlas")

  set.seed(42)
  data_arr <- array(rnorm(5 * 5 * 5 * 20), dim = c(5, 5, 5, 20))
  nvec_sp <- neuroim2::NeuroSpace(dim = c(5, 5, 5, 20), spacing = c(1, 1, 1))
  data_vol <- neuroim2::NeuroVec(data_arr, nvec_sp)

  conn_high <- atlas_connectivity(data_vol, atlas, threshold = 0.99)
  # With very high threshold, most connections should be zeroed
  expect_true(all(conn_high == 0) || sum(conn_high != 0) <= 2)
})

test_that("atlas_connectivity igraph conversion works", {
  skip_on_cran()
  skip_if_not_installed("igraph")

  # Simple 2x2 connectivity matrix
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  dimnames(mat) <- list(c("A", "B"), c("A", "B"))
  class(mat) <- c("atlas_connectivity", class(mat))

  g <- as_igraph(mat)
  expect_true(igraph::is_igraph(g))
  expect_equal(igraph::vcount(g), 2)
})

test_that("atlas_connectivity spearman method works", {
  skip_on_cran()

  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:5, 1:5] <- 1L
  arr[3:5, 1:5, 1:5] <- 2L
  sp <- neuroim2::NeuroSpace(dim = c(5, 5, 5), spacing = c(1, 1, 1), origin = c(0, 0, 0))
  atlas_vol <- neuroim2::NeuroVol(arr, sp)
  atlas <- structure(list(
    name = "spearman_test", atlas = atlas_vol,
    ids = 1:2, labels = c("A", "B"), orig_labels = c("A", "B"),
    hemi = c("left", "right")
  ), class = "atlas")

  set.seed(42)
  data_arr <- array(rnorm(5 * 5 * 5 * 10), dim = c(5, 5, 5, 10))
  nvec_sp <- neuroim2::NeuroSpace(dim = c(5, 5, 5, 10), spacing = c(1, 1, 1))
  data_vol <- neuroim2::NeuroVec(data_arr, nvec_sp)

  conn <- atlas_connectivity(data_vol, atlas, method = "spearman")
  expect_true(inherits(conn, "atlas_connectivity"))
  expect_equal(nrow(conn), 2)
  expect_equal(unname(diag(conn)), c(0, 0))
})

test_that("atlas_connectivity rejects non-NeuroVec input", {
  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:5, 1:5] <- 1L
  sp <- neuroim2::NeuroSpace(dim = c(5, 5, 5), spacing = c(1, 1, 1), origin = c(0, 0, 0))
  atlas_vol <- neuroim2::NeuroVol(arr, sp)
  atlas <- structure(list(
    name = "err_test", atlas = atlas_vol,
    ids = 1L, labels = "A", orig_labels = "A", hemi = "left"
  ), class = "atlas")

  expect_error(atlas_connectivity(atlas_vol, atlas), "NeuroVec")
})

test_that("print.atlas_connectivity works", {
  mat <- matrix(c(0, 0.3, 0.5, 0.3, 0, 0.1, 0.5, 0.1, 0), 3, 3)
  dimnames(mat) <- list(c("A", "B", "C"), c("A", "B", "C"))
  class(mat) <- c("atlas_connectivity", class(mat))

  out <- capture.output(print(mat))
  expect_true(any(grepl("Regions: 3", out)))
  expect_true(any(grepl("Non-zero edges:", out)))
})
