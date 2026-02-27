# Tests for atlas_hierarchy(), sub_atlas(network=), and reduce_atlas(level=, by=)

# Helper: create a toy atlas with hierarchy information
make_hierarchy_atlas <- function() {
  arr <- array(0L, dim = c(10, 10, 10))
  arr[1:5, 1:5, 1:5] <- 1L
  arr[6:10, 1:5, 1:5] <- 2L
  arr[1:5, 6:10, 1:5] <- 3L
  arr[6:10, 6:10, 1:5] <- 4L
  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))
  vol <- neuroim2::NeuroVol(arr, sp)
  structure(list(
    name = "hier_test",
    atlas = vol,
    ids = 1:4,
    labels = c("R1", "R2", "R3", "R4"),
    orig_labels = c("R1", "R2", "R3", "R4"),
    hemi = c("left", "right", "left", "right"),
    network = c("NetA", "NetA", "NetB", "NetB")
  ), class = "atlas")
}

# Helper: atlas without network
make_no_network_atlas <- function() {
  arr <- array(0L, dim = c(10, 10, 10))
  arr[1:5, 1:5, 1:5] <- 1L
  arr[6:10, 1:5, 1:5] <- 2L
  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))
  vol <- neuroim2::NeuroVol(arr, sp)
  structure(list(
    name = "no_net_test",
    atlas = vol,
    ids = 1:2,
    labels = c("A1", "A2"),
    orig_labels = c("A1", "A2"),
    hemi = c("left", "right"),
    network = NULL
  ), class = "atlas")
}

# --- atlas_hierarchy tests ---

test_that("atlas_hierarchy returns correct levels for atlas with network", {
  atlas <- make_hierarchy_atlas()
  h <- atlas_hierarchy(atlas)

  expect_s3_class(h, "atlas_hierarchy")
  expect_equal(h$levels, c("parcel", "network", "hemisphere"))
})

test_that("atlas_hierarchy returns only parcel+hemisphere for atlas without network", {
  atlas <- make_no_network_atlas()
  h <- atlas_hierarchy(atlas)

  expect_equal(h$levels, c("parcel", "hemisphere"))
  expect_null(h$mappings$network)
  expect_length(h$mappings$hemisphere, 2)
})

test_that("atlas_hierarchy mappings have correct structure", {
  atlas <- make_hierarchy_atlas()
  h <- atlas_hierarchy(atlas)

  # network mapping
  expect_length(h$mappings$network, 4)
  expect_equal(names(h$mappings$network), c("R1", "R2", "R3", "R4"))
  expect_equal(unname(h$mappings$network), c("NetA", "NetA", "NetB", "NetB"))

  # hemisphere mapping
  expect_length(h$mappings$hemisphere, 4)
  expect_equal(unname(h$mappings$hemisphere),
               c("left", "right", "left", "right"))
})

test_that("atlas_hierarchy errors on non-atlas input", {
  expect_error(atlas_hierarchy(list(a = 1)), "'atlas' must be an atlas object")
})

test_that("atlas_hierarchy print method works", {
  atlas <- make_hierarchy_atlas()
  h <- atlas_hierarchy(atlas)
  output <- capture.output(print(h))
  expect_true(any(grepl("Atlas Hierarchy", output)))
  expect_true(any(grepl("network: 2 groups", output)))
  expect_true(any(grepl("hemisphere: 2 groups", output)))
})

# --- sub_atlas with network= ---

test_that("sub_atlas with network= filter works", {
  atlas <- make_hierarchy_atlas()
  sub <- sub_atlas(atlas, network = "NetA")

  expect_equal(sub$ids, c(1L, 2L))
  expect_equal(sub$labels, c("R1", "R2"))
  expect_equal(sub$network, c("NetA", "NetA"))
})

test_that("sub_atlas network= errors when atlas has no network", {
  atlas <- make_no_network_atlas()
  expect_error(sub_atlas(atlas, network = "NetA"),
               "Atlas does not have network information")
})

test_that("sub_atlas network= combined with hemi works", {
  atlas <- make_hierarchy_atlas()
  sub <- sub_atlas(atlas, network = "NetA", hemi = "left")

  expect_equal(sub$ids, 1L)
  expect_equal(sub$labels, "R1")
})

# --- reduce_atlas with level= and by= ---

test_that("reduce_atlas with level='parcel' returns same as default", {
  atlas <- make_hierarchy_atlas()
  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))
  data_arr <- array(rnorm(1000), dim = c(10, 10, 10))
  data_vol <- neuroim2::NeuroVol(data_arr, sp)

  default_result <- reduce_atlas(atlas, data_vol, mean)
  parcel_result <- reduce_atlas(atlas, data_vol, mean, level = "parcel")

  expect_equal(default_result, parcel_result)
})

test_that("reduce_atlas with level='network' aggregates correctly", {
  atlas <- make_hierarchy_atlas()
  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))

  # Create data where each region has a known constant value
  data_arr <- array(0, dim = c(10, 10, 10))
  data_arr[1:5, 1:5, 1:5] <- 10   # R1 = 10
  data_arr[6:10, 1:5, 1:5] <- 20  # R2 = 20
  data_arr[1:5, 6:10, 1:5] <- 30  # R3 = 30
  data_arr[6:10, 6:10, 1:5] <- 40 # R4 = 40
  data_vol <- neuroim2::NeuroVol(data_arr, sp)

  result <- reduce_atlas(atlas, data_vol, mean, level = "network")

  expect_true("value" %in% names(result))
  expect_true("network" %in% names(result))
  expect_equal(nrow(result), 2)

  # NetA = mean of R1(10) and R2(20) = 15
  # NetB = mean of R3(30) and R4(40) = 35
  neta_val <- result$value[result$network == "NetA"]
  netb_val <- result$value[result$network == "NetB"]
  expect_equal(neta_val, 15)
  expect_equal(netb_val, 35)
})

test_that("reduce_atlas with by= formula works", {
  atlas <- make_hierarchy_atlas()
  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))

  data_arr <- array(0, dim = c(10, 10, 10))
  data_arr[1:5, 1:5, 1:5] <- 10   # R1
  data_arr[6:10, 1:5, 1:5] <- 20  # R2
  data_arr[1:5, 6:10, 1:5] <- 30  # R3
  data_arr[6:10, 6:10, 1:5] <- 40 # R4
  data_vol <- neuroim2::NeuroVol(data_arr, sp)

  result <- reduce_atlas(atlas, data_vol, mean, by = ~ hemi)

  expect_true("value" %in% names(result))
  expect_true("hemi" %in% names(result))
  expect_equal(nrow(result), 2)

  # left = mean of R1(10) and R3(30) = 20
  # right = mean of R2(20) and R4(40) = 30
  left_val <- result$value[result$hemi == "left"]
  right_val <- result$value[result$hemi == "right"]
  expect_equal(left_val, 20)
  expect_equal(right_val, 30)
})

test_that("reduce_atlas errors when level='network' on atlas without network", {
  atlas <- make_no_network_atlas()
  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))
  data_arr <- array(rnorm(1000), dim = c(10, 10, 10))
  data_vol <- neuroim2::NeuroVol(data_arr, sp)

  expect_error(reduce_atlas(atlas, data_vol, mean, level = "network"),
               "Atlas does not have network information")
})

test_that("reduce_atlas by= errors with non-formula argument", {
  atlas <- make_hierarchy_atlas()
  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))
  data_arr <- array(rnorm(1000), dim = c(10, 10, 10))
  data_vol <- neuroim2::NeuroVol(data_arr, sp)

  expect_error(reduce_atlas(atlas, data_vol, mean, by = "network"),
               "'by' must be a formula")
})

test_that("reduce_atlas by= errors with unknown variable", {
  atlas <- make_hierarchy_atlas()
  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))
  data_arr <- array(rnorm(1000), dim = c(10, 10, 10))
  data_vol <- neuroim2::NeuroVol(data_arr, sp)

  expect_error(reduce_atlas(atlas, data_vol, mean, by = ~ nonexistent),
               "Variables not found in roi_metadata")
})
