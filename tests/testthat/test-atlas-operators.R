test_that("%where% produces same result as filter_atlas", {
  # Create a toy atlas with network info
  arr <- array(0L, dim = c(10, 10, 10))
  arr[1:3, 1:3, 1:3] <- 1L
  arr[5:7, 5:7, 5:7] <- 2L
  arr[1:3, 5:7, 1:3] <- 3L
  arr[5:7, 1:3, 5:7] <- 4L

  sp <- neuroim2::NeuroSpace(dim = c(10,10,10), spacing = c(1,1,1), origin = c(0,0,0))
  vol <- neuroim2::NeuroVol(arr, sp)

  atlas <- structure(list(
    name = "test_op",
    atlas = vol,
    ids = 1:4,
    labels = c("RegA", "RegB", "RegC", "RegD"),
    orig_labels = c("RegA", "RegB", "RegC", "RegD"),
    hemi = c("left", "right", "left", "right"),
    network = c("Vis", "Vis", "DMN", "DMN")
  ), class = "atlas")

  # Test basic filter
  result_where <- atlas %where% (hemi == "left")
  result_filter <- filter_atlas(atlas, hemi == "left")
  expect_identical(result_where$ids, result_filter$ids)
  expect_identical(result_where$labels, result_filter$labels)

  # Test compound expression
  result_where2 <- atlas %where% (network == "DMN" & hemi == "left")
  result_filter2 <- filter_atlas(atlas, network == "DMN" & hemi == "left")
  expect_identical(result_where2$ids, result_filter2$ids)
})

test_that("%where% errors on non-atlas input", {
  expect_error(42 %where% (x > 1), "atlas object")
})

test_that("%where% works with grepl", {
  arr <- array(0L, dim = c(10, 10, 10))
  arr[1:3, 1:3, 1:3] <- 1L
  arr[5:7, 5:7, 5:7] <- 2L

  sp <- neuroim2::NeuroSpace(dim = c(10,10,10), spacing = c(1,1,1), origin = c(0,0,0))
  vol <- neuroim2::NeuroVol(arr, sp)

  atlas <- structure(list(
    name = "test_grepl",
    atlas = vol,
    ids = 1:2,
    labels = c("VisualA", "MotorB"),
    orig_labels = c("VisualA", "MotorB"),
    hemi = c("left", "right")
  ), class = "atlas")

  result <- atlas %where% grepl("Visual", label)
  expect_equal(result$ids, 1L)
  expect_equal(result$labels, "VisualA")
})
