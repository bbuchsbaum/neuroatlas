test_that("reduce_atlas handles various data types and edge cases correctly", {
  skip_on_cran()
  
  # Load test atlas
  atlas <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7", resolution = "2")
  }, error = function(e) {
    skip("Failed to download Schaefer atlas")
  })
  
  # Create test data matching atlas dimensions
  atlas_dims <- dim(atlas$atlas)
  atlas_space <- neuroim2::space(atlas$atlas)
  
  # Test 1: Basic 3D data reduction
  # Create random data with known properties
  set.seed(123)
  test_data_3d <- neuroim2::NeuroVol(
    rnorm(prod(atlas_dims), mean = 10, sd = 2),
    space = atlas_space
  )
  
  # Test mean reduction
  result_mean <- reduce_atlas(atlas, test_data_3d, mean)
  
  # Critical checks - default is long format for NeuroVol
  expect_true(inherits(result_mean, "tbl_df"))
  expect_equal(nrow(result_mean), 100)  # Should have one row per region
  expect_true(all(c("region", "value") %in% names(result_mean)))
  
  # Values should be reasonable (around mean=10)
  expect_true(all(result_mean$value > 5 & result_mean$value < 15))
  expect_equal(length(unique(result_mean$region)), 100)
  
  # Test with custom function that tracks calls
  call_count <- 0
  custom_func <- function(x, ...) {
    call_count <<- call_count + 1
    median(x, na.rm = TRUE)
  }
  
  result_custom <- reduce_atlas(atlas, test_data_3d, custom_func)
  expect_equal(call_count, 100)  # Should be called once per region
  
  # Test 2: 4D time series data using NeuroVec
  vol_list <- lapply(1:3, function(i) {
    neuroim2::NeuroVol(
      rnorm(prod(atlas_dims), mean = 5 + i),
      space = atlas_space
    )
  })
  vec4d <- neuroim2::NeuroVec(vol_list)

  result_vec <- reduce_atlas(atlas, vec4d, mean)
  expect_true(inherits(result_vec, "tbl_df"))
  expect_equal(nrow(result_vec), 3)
  expect_equal(ncol(result_vec), length(atlas$orig_labels) + 1)  # time + regions
  
  # Test 3: Edge cases and error conditions
  # Empty region handling - create data where one region might be empty
  sparse_data <- neuroim2::NeuroVol(
    numeric(prod(atlas_dims)),  # All zeros
    space = atlas_space
  )
  
  # Set values in the middle of the brain where regions actually exist
  # Use coordinates around the median brain location
  sparse_data[40:50, 50:60, 40:50] <- 1
  
  # Default is long format for NeuroVol
  result_sparse <- reduce_atlas(atlas, sparse_data, sum)
  
  # Most regions should have sum = 0
  expect_true(sum(result_sparse$value == 0) > 80)  # Allow for more non-zero regions
  # At least some regions should have non-zero values
  expect_true(any(result_sparse$value > 0))
  
  # Test error handling
  # Wrong space
  wrong_space <- neuroim2::NeuroSpace(
    dim = c(50, 50, 50),
    spacing = c(3, 3, 3),
    origin = c(0, 0, 0)
  )
  wrong_data <- neuroim2::NeuroVol(
    rnorm(50^3),
    space = wrong_space
  )
  
  expect_error(reduce_atlas(atlas, wrong_data, mean),
               "dimension|space|compatible")
  
  # Non-function stat_func
  expect_error(reduce_atlas(atlas, test_data_3d, "not_a_function"),
               "'stat_func' must be a function")
  
  # Test with NA handling
  na_data <- test_data_3d
  # Get the first region's ID and label
  first_id <- atlas$ids[1]
  first_label <- atlas$orig_labels[1]

  # For ClusteredNeuroVol, we need to find where the first region is
  if (inherits(atlas$atlas, "ClusteredNeuroVol")) {
    # Get dense representation to find the first region
    atlas_dense <- neuroim2::as.dense(atlas$atlas)
    # Create a copy of the data and set NA where first region is
    na_data_vals <- na_data[,,]
    na_data_vals[atlas_dense == first_id] <- NA
    na_data <- neuroim2::NeuroVol(na_data_vals, space = neuroim2::space(na_data))
  } else {
    na_data[atlas$atlas == first_id] <- NA
  }

  result_na <- reduce_atlas(atlas, na_data, mean, na.rm = TRUE)
  # Find the row for the first region in long format (using orig_labels)
  region_1_row <- which(result_na$region == first_label)
  expect_true(is.na(result_na$value[region_1_row]))
  
  # Other regions should have valid values
  expect_false(all(is.na(result_na$value)))
})

test_that("get_roi extracts regions correctly and handles edge cases", {
  skip_on_cran()
  
  # Load atlas with known regions
  atlas <- get_aseg_atlas()
  
  # Test 1: Single region extraction by label
  hippo <- get_roi(atlas, label = "Hippocampus")
  
  expect_true(is.list(hippo))
  # ASEG atlas has bilateral structures, so Hippocampus returns 2 ROIs (left and right)
  expect_true(length(hippo) >= 1)
  expect_true(all(names(hippo) == "Hippocampus"))
  expect_true(all(sapply(hippo, function(x) inherits(x, "ROIVol"))))
  
  # Verify the ROI contains data
  total_roi_voxels <- sum(sapply(hippo, function(x) nrow(x@coords)))
  expect_true(total_roi_voxels > 0)
  
  # If we have multiple hippocampi, verify they match the atlas IDs
  hippo_ids <- atlas$ids[which(atlas$labels == "Hippocampus")]
  if (length(hippo_ids) > 0) {
    if (inherits(atlas$atlas, "ClusteredNeuroVol")) {
      atlas_hippo_voxels <- sum(atlas$atlas@clusters %in% hippo_ids)
    } else {
      atlas_data <- if (inherits(atlas$atlas, "NeuroVol")) atlas$atlas[,,] else atlas$atlas
      atlas_hippo_voxels <- suppressWarnings(sum(atlas_data %in% hippo_ids))
    }
    # Allow some tolerance for resampling differences
    expect_true(abs(total_roi_voxels - atlas_hippo_voxels) / atlas_hippo_voxels < 0.05 || 
                total_roi_voxels == atlas_hippo_voxels)
  }
  
  # Test 2: Multiple region extraction
  regions <- get_roi(atlas, label = c("Hippocampus", "Amygdala", "Thalamus"))
  
  expect_equal(length(regions), 3)
  expect_equal(sort(names(regions)), sort(c("Hippocampus", "Amygdala", "Thalamus")))
  
  # Each should be a valid ROIVol
  for (roi in regions) {
    expect_true(inherits(roi, "ROIVol"))
    expect_true(length(roi@coords) > 0)
  }
  
  # Test 3: Extraction by ID
  thalamus_ids <- atlas$ids[which(atlas$labels == "Thalamus")]
  thalamus_by_id <- get_roi(atlas, id = thalamus_ids)
  
  # Should get both left and right thalamus
  expect_equal(length(thalamus_by_id), length(thalamus_ids))
  expect_equal(names(thalamus_by_id), as.character(thalamus_ids))
  
  # Should extract the same total voxels as by label
  thalamus_by_label <- get_roi(atlas, label = "Thalamus")
  total_voxels_by_id <- sum(sapply(thalamus_by_id, function(x) nrow(x@coords)))
  total_voxels_by_label <- sum(sapply(thalamus_by_label, function(x) nrow(x@coords)))
  expect_equal(total_voxels_by_id, total_voxels_by_label)
  
  # Test error conditions
  # Non-existent label
  expect_error(get_roi(atlas, label = "NonExistentRegion"),
               "label.*not found|does not exist")
  
  # Both label and id specified
  expect_error(get_roi(atlas, label = "Hippocampus", id = 1),
               "must supply one of.*but not both")
  
  # Invalid ID - should return empty ROIVol
  result <- get_roi(atlas, id = 9999)
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]@coords), 0)
  
  # Test hemisphere filtering (if implemented)
  # Some atlases might support hemisphere-specific extraction
  if ("hemi" %in% names(formals(get_roi.atlas))) {
    left_labels <- atlas$labels[which(atlas$hemi == "left")]
    if (length(left_labels) > 0) {
      left_regions <- get_roi(atlas, label = left_labels)
      expect_true(length(left_regions) > 0)
    }
  }
})

test_that("map_atlas correctly maps values and applies thresholds", {
  skip_on_cran()
  
  # Test with ASEG atlas which doesn't require optional dependencies
  atlas <- tryCatch({
    get_aseg_atlas()
  }, error = function(e) {
    skip("Failed to download ASEG atlas")
  })
  
  # Test 1: Basic value mapping
  set.seed(456)
  n_regions <- length(atlas$orig_labels)
  test_values <- rnorm(n_regions, mean = 0, sd = 1)
  
  mapped <- map_atlas(atlas, vals = test_values, thresh = c(0, Inf))
  
  expect_true(inherits(mapped, "tbl_df"))
  # For non-ggseg atlases, we get exactly as many rows as input values
  expect_equal(nrow(mapped), n_regions)
  expect_true(all(c("statistic", "label") %in% names(mapped)))
  
  # Values should match input exactly for non-ggseg atlases
  expect_equal(mapped$statistic, test_values)
  expect_equal(mapped$label, atlas$orig_labels)
  
  # Test 2: Threshold application
  # Only keep values with absolute value > 1.5
  mapped_thresh <- map_atlas(atlas, vals = test_values, 
                            thresh = c(1.5, Inf), pos = FALSE)
  
  # Check thresholding works - values with |val| <= 1.5 should become NA
  # Get the non-NA statistics
  non_na_thresh <- mapped_thresh$statistic[!is.na(mapped_thresh$statistic)]
  # All remaining values should have |val| > 1.5
  expect_true(all(abs(non_na_thresh) > 1.5))
  
  # Test 3: Positive-only thresholding
  mapped_pos <- map_atlas(atlas, vals = test_values, 
                         thresh = c(1, 2), pos = TRUE)
  
  # Check that only values in range (1, 2] are kept
  non_na_pos <- mapped_pos$statistic[!is.na(mapped_pos$statistic)]
  expect_true(all(non_na_pos > 1 & non_na_pos <= 2))
  
  # Test edge cases
  # Wrong number of values
  expect_error(map_atlas(atlas, vals = rnorm(50), thresh = c(0, 0)))
  
  # Special values
  special_vals <- test_values
  special_vals[1] <- Inf
  special_vals[2] <- -Inf
  special_vals[3] <- NA
  special_vals[4] <- NaN
  
  mapped_special <- map_atlas(atlas, vals = special_vals, thresh = c(0, 0))
  
  # Should handle special values gracefully
  expect_true(is.na(mapped_special$statistic[1]) || 
              mapped_special$statistic[1] == Inf)
  expect_true(is.na(mapped_special$statistic[2]) || 
              mapped_special$statistic[2] == -Inf)
  expect_true(is.na(mapped_special$statistic[3]))
  expect_true(is.na(mapped_special$statistic[4]))
  
  # Skip Glasser test as it requires ggsegGlasser
  # Test with Glasser atlas would go here but requires optional dependency
})

test_that("map_atlas.schaefer delegates to map_atlas.atlas", {
  dummy_atlas <- structure(
    list(name = "schaefer_dummy", orig_labels = c("a", "b", "c"),
         labels = c("a", "b", "c"), hemi = c("left", "left", "right"),
         ids = 1:3),
    class = c("schaefer", "atlas")
  )
  vals <- c(10, 20, 30)

  result <- map_atlas(dummy_atlas, vals = vals)
  expect_s3_class(result, "tbl_df")
  expect_equal(result$statistic, vals)
  expect_equal(result$label, c("a", "b", "c"))
})

test_that("map_atlas.glasser delegates to map_atlas.atlas", {
  dummy_atlas <- structure(
    list(labels = "V1", orig_labels = "L_V1", hemi = "left", ids = 1L),
    class = c("glasser", "atlas")
  )

  result <- map_atlas(dummy_atlas, vals = 42)
  expect_s3_class(result, "tbl_df")
  expect_equal(result$statistic, 42)
  expect_equal(result$label, "L_V1")
})
