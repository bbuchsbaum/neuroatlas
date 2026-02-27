# tests/testthat/test-atlas-graph.R

make_graph_atlas <- function() {
  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:5, 1:5] <- 1L
  arr[3:3, 1:5, 1:5] <- 2L
  arr[4:5, 1:5, 1:5] <- 3L

  sp  <- neuroim2::NeuroSpace(dim = c(5, 5, 5), spacing = c(1, 1, 1),
                               origin = c(0, 0, 0))
  vol <- neuroim2::NeuroVol(arr, sp)
  structure(list(
    name        = "graph_test",
    atlas       = vol,
    ids         = 1:3,
    labels      = c("Left", "Middle", "Right"),
    orig_labels = c("Left", "Middle", "Right"),
    hemi        = c("left", NA, "right")
  ), class = "atlas")
}

test_that("atlas_graph returns a square symmetric matrix", {
  atl <- make_graph_atlas()
  adj <- atlas_graph(atl)

  expect_true(is.matrix(adj))
  expect_equal(nrow(adj), 3L)
  expect_equal(ncol(adj), 3L)
  expect_equal(adj, t(adj))
})

test_that("known adjacency: 1-2 and 2-3 adjacent, 1-3 not", {
  atl <- make_graph_atlas()
  adj <- atlas_graph(atl, connectivity = "6")

  expect_true(adj["Left", "Middle"] > 0)
  expect_true(adj["Middle", "Right"] > 0)
  expect_equal(adj["Left", "Right"], 0L)
})

test_that("weights are positive counts where parcels are adjacent", {
  atl <- make_graph_atlas()
  adj <- atlas_graph(atl, connectivity = "6")

  # boundary between slab x=2 and x=3 is 5*5 = 25 face pairs

  expect_equal(adj["Left", "Middle"], 25L)
  expect_equal(adj["Middle", "Right"], 25L)
})

test_that("include_weight = FALSE gives binary matrix", {
  atl <- make_graph_atlas()
  adj <- atlas_graph(atl, include_weight = FALSE)

  expect_true(all(adj %in% c(0L, 1L)))
  expect_equal(adj["Left", "Middle"], 1L)
})

test_that("tibble output has expected columns", {
  atl <- make_graph_atlas()
  el  <- atlas_graph(atl, as = "tibble")

  expect_true(is.data.frame(el))
  expect_true(all(c("from", "to", "weight") %in% names(el)))
  expect_equal(nrow(el), 2L)
})

test_that("igraph output works", {
  skip_if_not_installed("igraph")
  atl <- make_graph_atlas()
  g   <- atlas_graph(atl, as = "igraph")

  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 3L)
  expect_equal(igraph::ecount(g), 2L)
})

test_that("connectivity 18 finds more neighbours than 6", {
  # Build atlas where two parcels share only an edge, not a face
  arr <- array(0L, dim = c(4, 4, 4))
  arr[1:2, 1:2, 1:4] <- 1L
  arr[3:4, 3:4, 1:4] <- 2L

  sp  <- neuroim2::NeuroSpace(dim = c(4, 4, 4), spacing = c(1, 1, 1),
                               origin = c(0, 0, 0))
  vol <- neuroim2::NeuroVol(arr, sp)
  atl <- structure(list(
    name        = "edge_test",
    atlas       = vol,
    ids         = 1:2,
    labels      = c("A", "B"),
    orig_labels = c("A", "B"),
    hemi        = c(NA, NA)
  ), class = "atlas")

  adj6  <- atlas_graph(atl, connectivity = "6")
  adj18 <- atlas_graph(atl, connectivity = "18")

  # face-adjacency: these parcels don't share a face

  expect_equal(adj6["A", "B"], 0L)
  # edge-adjacency: they share an edge at (2,2,z)-(3,3,z)

  expect_true(adj18["A", "B"] > 0)
})

test_that("dimnames use atlas labels", {
  atl <- make_graph_atlas()
  adj <- atlas_graph(atl)

  expect_equal(rownames(adj), c("Left", "Middle", "Right"))
  expect_equal(colnames(adj), c("Left", "Middle", "Right"))
})
