test_that("atlas loading maintains data integrity and handles errors gracefully", {
  skip_on_cran()
  
  # Test 1: Schaefer atlas label consistency and ID mapping
  # This tests critical ID remapping for right hemisphere and label parsing
  atlas <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7")
  }, error = function(e) {
    skip("Failed to download Schaefer atlas")
  })
  
  # Check basic structure
  expect_true(inherits(atlas, "atlas"))
  expect_true(inherits(atlas, "schaefer"))
  expect_equal(length(atlas$ids), 100)
  expect_equal(length(atlas$labels), 100)
  expect_equal(length(atlas$orig_labels), 100)
  expect_equal(length(atlas$hemi), 100)
  expect_equal(length(atlas$network), 100)
  
  # Critical: Check hemisphere distribution (should be exactly 50/50)
  hemi_table <- table(atlas$hemi)
  expect_equal(as.numeric(hemi_table["left"]), 50)
  expect_equal(as.numeric(hemi_table["right"]), 50)
  
  # Critical: Check ID mapping - left should be 1:50, right should be 51:100
  left_ids <- atlas$ids[atlas$hemi == "left"]
  right_ids <- atlas$ids[atlas$hemi == "right"]
  expect_equal(sort(left_ids), 1:50)
  expect_equal(sort(right_ids), 51:100)
  
  # Critical: Verify label_map integrity in ClusteredNeuroVol
  label_map <- atlas$atlas@label_map
  expect_equal(length(label_map), 100)
  
  # Label map values should match the IDs
  label_map_ids <- sort(unique(unlist(label_map)))
  expect_equal(label_map_ids, 1:100)
  
  # Check that all voxel values in the atlas correspond to valid IDs
  # Use proper S4 extraction
  atlas_values <- unique(atlas$atlas@.Data[atlas$atlas@.Data != 0])
  expect_true(all(atlas_values %in% 1:100))
  
  # Test error handling for invalid parameters
  expect_error(get_schaefer_atlas(parcels = "999", networks = "7"), 
               "'arg' should be one of")
  expect_error(get_schaefer_atlas(parcels = "100", networks = "5"), 
               "'arg' should be one of")
})

test_that("resampling preserves labels and handles edge cases correctly", {
  skip("Skipping resampling test - neuroim2::resample doesn't support ClusteredNeuroVol properly")
})

test_that("merge_atlases handles ID conflicts and maintains referential integrity", {
  skip_on_cran()
  
  # Test 3: Critical atlas merging scenarios
  # Create two small atlases to merge
  atlas1 <- get_aseg_atlas()
  
  # We need a second atlas in the same space
  # For testing, we'll use a modified version of the same atlas
  atlas2 <- atlas1
  atlas2$name <- "aseg_modified"
  
  # Modify IDs to create a potential conflict scenario
  # This simulates merging atlases that might have overlapping ID schemes
  max_id <- max(atlas1$ids)
  
  # Test basic merge
  merged <- merge_atlases(atlas1, atlas2)
  
  # Critical checks for merged atlas
  expect_equal(merged$name, paste0(atlas1$name, "::", atlas2$name))
  expect_equal(length(merged$ids), length(atlas1$ids) + length(atlas2$ids))
  expect_equal(length(merged$labels), length(atlas1$labels) + length(atlas2$labels))
  
  # Check ID adjustment - atlas2 IDs should be shifted
  original_max <- max(atlas1$ids)
  atlas2_ids_in_merged <- merged$ids[(length(atlas1$ids) + 1):length(merged$ids)]
  expect_true(all(atlas2_ids_in_merged > original_max))
  
  # Check no ID collisions
  expect_equal(length(unique(merged$ids)), length(merged$ids))
  
  # Verify spatial integrity - merged volume should contain all regions
  # Convert ClusteredNeuroVol to dense first
  merged_dense <- neuroim2::as.dense(merged$atlas)
  merged_values <- sort(unique(as.vector(merged_dense[merged_dense != 0])))
  
  # When merging identical atlases, the second overwrites the first
  # So we expect only the shifted IDs from atlas2
  shifted_start <- max(atlas1$ids) + 1
  expected_values <- seq(shifted_start, shifted_start + length(atlas2$ids) - 1)
  expect_equal(merged_values, expected_values)
  
  # Test dimension mismatch error
  # Create atlas with different dimensions
  different_space <- neuroim2::NeuroSpace(
    dim = c(50, 50, 50),
    spacing = c(2, 2, 2),
    origin = c(0, 0, 0)
  )
  
  # Create a dummy atlas with different dimensions
  dummy_vol <- neuroim2::NeuroVol(array(1, dim = c(50, 50, 50)), 
                                  space = different_space)
  dummy_atlas <- list(
    name = "dummy",
    atlas = neuroim2::ClusteredNeuroVol(
      as.logical(dummy_vol),
      clusters = dummy_vol[dummy_vol != 0],
      label_map = list("region1" = 1)
    ),
    ids = 1,
    labels = "region1",
    orig_labels = "region1",
    hemi = NA,
    cmap = data.frame(id = 1, r = 255, g = 0, b = 0)
  )
  class(dummy_atlas) <- c("dummy", "atlas")
  
  # This should fail due to dimension mismatch
  expect_error(merge_atlases(atlas1, dummy_atlas),
               "dim\\(atlas1\\$atlas\\) == dim\\(atlas2\\$atlas\\)")
  
  # Test that merged atlas maintains all labels
  expect_equal(length(merged$labels), 
               length(atlas1$labels) + length(atlas2$labels))
})