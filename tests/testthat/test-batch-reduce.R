test_that("batch_reduce combines multiple subjects", {
  skip_on_cran()

  # Create toy atlas
  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:5, 1:5] <- 1L
  arr[3:5, 1:5, 1:5] <- 2L
  sp <- neuroim2::NeuroSpace(dim = c(5,5,5), spacing = c(1,1,1), origin = c(0,0,0))
  atlas_vol <- neuroim2::NeuroVol(arr, sp)
  atlas <- structure(list(
    name = "batch_test", atlas = atlas_vol,
    ids = 1:2, labels = c("RegA", "RegB"),
    orig_labels = c("RegA", "RegB"),
    hemi = c("left", "right")
  ), class = "atlas")

  # Create 3 toy NeuroVols
  set.seed(42)
  vols <- list(
    sub01 = neuroim2::NeuroVol(array(rnorm(125), c(5,5,5)), sp),
    sub02 = neuroim2::NeuroVol(array(rnorm(125), c(5,5,5)), sp),
    sub03 = neuroim2::NeuroVol(array(rnorm(125), c(5,5,5)), sp)
  )

  result <- batch_reduce(vols, atlas, mean, .progress = FALSE)

  # Check structure
  expect_true("subject" %in% names(result))
  expect_equal(unique(result$subject), c("sub01", "sub02", "sub03"))

  # Each subject should have 2 regions in long format
  expect_equal(nrow(result), 6)  # 3 subjects x 2 regions
})

test_that("batch_reduce auto-names unnamed inputs", {
  skip_on_cran()

  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:5, 1:5] <- 1L
  arr[3:5, 1:5, 1:5] <- 2L
  sp <- neuroim2::NeuroSpace(dim = c(5,5,5), spacing = c(1,1,1), origin = c(0,0,0))
  atlas_vol <- neuroim2::NeuroVol(arr, sp)
  atlas <- structure(list(
    name = "batch_test", atlas = atlas_vol,
    ids = 1:2, labels = c("RegA", "RegB"),
    orig_labels = c("RegA", "RegB"),
    hemi = c("left", "right")
  ), class = "atlas")

  # Unnamed list
  vols <- list(
    neuroim2::NeuroVol(array(1, c(5,5,5)), sp),
    neuroim2::NeuroVol(array(2, c(5,5,5)), sp)
  )

  result <- batch_reduce(vols, atlas, mean, .progress = FALSE)
  expect_equal(unique(result$subject), c("sub_001", "sub_002"))
})

test_that("batch_reduce parallel works", {
  skip_on_cran()
  skip_if_not_installed("future.apply")

  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:5, 1:5] <- 1L
  arr[3:5, 1:5, 1:5] <- 2L
  sp <- neuroim2::NeuroSpace(dim = c(5,5,5), spacing = c(1,1,1), origin = c(0,0,0))
  atlas_vol <- neuroim2::NeuroVol(arr, sp)
  atlas <- structure(list(
    name = "batch_test", atlas = atlas_vol,
    ids = 1:2, labels = c("RegA", "RegB"),
    orig_labels = c("RegA", "RegB"),
    hemi = c("left", "right")
  ), class = "atlas")

  set.seed(42)
  vols <- list(
    sub01 = neuroim2::NeuroVol(array(rnorm(125), c(5,5,5)), sp),
    sub02 = neuroim2::NeuroVol(array(rnorm(125), c(5,5,5)), sp)
  )

  result_seq <- batch_reduce(vols, atlas, mean, .progress = FALSE)
  result_par <- batch_reduce(vols, atlas, mean, parallel = TRUE, .progress = FALSE)

  expect_equal(nrow(result_par), nrow(result_seq))
  expect_equal(sort(result_par$subject), sort(result_seq$subject))
})
