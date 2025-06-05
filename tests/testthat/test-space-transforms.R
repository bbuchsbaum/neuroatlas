test_that("atlas resampling to TemplateFlow spaces maintains integrity", {
  skip_on_cran()
  reticulate::py_config()
  skip_if_not(reticulate::py_available(initialize = FALSE),
              "Python not available")
  skip_if_not(reticulate::py_module_available("templateflow"),
              "templateflow not available")

  # Test 1: Resample atlas to TemplateFlow space string
  atlas <- get_schaefer_atlas(parcels = "100", networks = "7", resolution = "2")

  # Test resampling to MNI space (string identifier)
  atlas_mni <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7",
                      outspace = "MNI152NLin2009cAsym")
  }, error = function(e) {
    skip(paste("TemplateFlow resolution failed:", e$message))
  })

  # Basic structure should be preserved
  expect_true(inherits(atlas_mni, "schaefer"))
  expect_equal(length(atlas_mni$ids), 100)
  expect_equal(length(atlas_mni$labels), 100)

  # Dimensions should match MNI template (typically 193x229x193 for 1mm)
  # But actual dimensions depend on the template resolution
  expect_equal(length(dim(atlas_mni$atlas)), 3)

  # Test 2: Resample with explicit resolution
  atlas_mni_2mm <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7",
                      outspace = list(space = "MNI152NLin2009cAsym",
                                    resolution = "2"))
  }, error = function(e) {
    skip(paste("TemplateFlow resolution failed:", e$message))
  })

  if (!is.null(atlas_mni_2mm)) {
    # Should have smaller dimensions than 1mm
    expect_true(all(dim(atlas_mni_2mm$atlas) < dim(atlas_mni$atlas)))
  }

  # Test 3: Error handling for invalid TemplateFlow spaces
  expect_error(
    get_schaefer_atlas(parcels = "100", networks = "7",
                      outspace = "InvalidSpaceName123"),
    "Failed to resolve.*TemplateFlow|not.*valid.*space"
  )

  # Test with incompatible space object
  tiny_space <- neuroim2::NeuroSpace(
    dim = c(10, 10, 10),  # Too small for meaningful atlas
    spacing = c(10, 10, 10),
    origin = c(0, 0, 0),
    axes = diag(3)
  )

  atlas_tiny <- get_schaefer_atlas(parcels = "100", networks = "7",
                                  outspace = tiny_space)

  # Should complete but likely lose many regions
  unique_regions <- length(unique(as.vector(atlas_tiny$atlas[atlas_tiny$atlas != 0])))
  expect_true(unique_regions < 100)  # Some regions will be lost
})

test_that("dilate_atlas handles masks correctly and preserves label integrity", {
  skip_on_cran()

  # Load atlas
  atlas <- get_aseg_atlas()

  # Test 1: Dilation with explicit mask
  # Create a restrictive mask (smaller than full brain)
  atlas_vol <- atlas$atlas
  full_mask <- as.logical(atlas_vol != 0)

  # Create mask that excludes some boundary voxels
  restrictive_mask <- full_mask
  dims <- dim(atlas_vol)
  # Zero out edges
  restrictive_mask[c(1:5, (dims[1]-4):dims[1]), , ] <- FALSE
  restrictive_mask[, c(1:5, (dims[2]-4):dims[2]), ] <- FALSE
  restrictive_mask[, , c(1:5, (dims[3]-4):dims[3])] <- FALSE

  restrictive_mask_vol <- neuroim2::NeuroVol(
    as.numeric(restrictive_mask),
    space = neuroim2::space(atlas_vol)
  )

  dilated <- dilate_atlas(atlas, mask = restrictive_mask_vol,
                         radius = 2, maxn = 10)

  # Check that dilation occurred
  original_voxels <- sum(atlas_vol != 0)
  dilated_voxels <- sum(dilated$atlas != 0)
  expect_true(dilated_voxels >= original_voxels)

  # Check that dilation respected the mask
  dilated_outside_mask <- sum(dilated$atlas != 0 & !restrictive_mask)
  expect_equal(dilated_outside_mask, 0)

  # Test 2: Label preservation during dilation
  # No new labels should be introduced
  original_labels <- sort(unique(as.vector(atlas_vol[atlas_vol != 0])))
  dilated_labels <- sort(unique(as.vector(dilated$atlas[dilated$atlas != 0])))
  expect_true(all(dilated_labels %in% original_labels))

  # If original atlas had a label_map, it should be preserved
  if (inherits(atlas$atlas, "ClusteredNeuroVol") && !is.null(atlas$atlas@label_map)) {
    expect_equal(dilated$atlas@label_map, atlas$atlas@label_map)
  }

  # Test 3: Edge cases
  # Very large radius (should be limited by mask)
  dilated_large <- dilate_atlas(atlas, mask = restrictive_mask_vol,
                               radius = 50, maxn = 100)

  # Still shouldn't exceed mask
  dilated_outside <- sum(dilated_large$atlas != 0 & !restrictive_mask)
  expect_equal(dilated_outside, 0)

  # Empty dilation (mask excludes all potential dilation targets)
  # Create mask that perfectly matches current atlas
  exact_mask <- neuroim2::NeuroVol(
    as.numeric(atlas_vol != 0),
    space = neuroim2::space(atlas_vol)
  )

  dilated_none <- dilate_atlas(atlas, mask = exact_mask,
                              radius = 5, maxn = 50)

  # Should return unchanged atlas
  expect_equal(sum(dilated_none$atlas != 0), sum(atlas_vol != 0))
  expect_equal(class(dilated_none), class(atlas))

  # Test with TemplateFlow mask string (if available)
  if (reticulate::py_available(initialize = FALSE) &&
      reticulate::py_module_available("templateflow")) {
    dilated_tf <- tryCatch({
      dilate_atlas(atlas, mask = "MNI152NLin2009cAsym",
                  radius = 1, maxn = 10)
    }, error = function(e) {
      # Skip if TemplateFlow fails
      NULL
    })

    if (!is.null(dilated_tf)) {
      expect_true(inherits(dilated_tf, "atlas"))
      expect_true(sum(dilated_tf$atlas != 0) >= sum(atlas_vol != 0))
    }
  }
})

test_that("cross-atlas operations maintain consistency", {
  skip_on_cran()

  # Test operations between different atlas types
  # Load different atlases
  aseg <- get_aseg_atlas()

  # Get a Schaefer atlas - try to get it in the same space
  schaefer <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7",
                      outspace = neuroim2::space(aseg$atlas))
  }, error = function(e) {
    # If that fails, get default and check if we can proceed
    schaefer_default <- get_schaefer_atlas(parcels = "100", networks = "7")
    if (!all(dim(aseg$atlas) == dim(schaefer_default$atlas))) {
      skip("Cannot get Schaefer in ASEG space - dimensions don't match")
    }
    schaefer_default
  })

  # Test 1: Merge cortical and subcortical
  merged <- merge_atlases(aseg, schaefer)

  # Total regions should be sum of both
  expect_equal(length(merged$ids),
               length(aseg$ids) + length(schaefer$ids))

  # Check spatial overlap (subcortical and cortical should not overlap)
  aseg_mask <- as.logical(as.vector(aseg$atlas != 0))
  schaefer_mask <- as.logical(as.vector(schaefer$atlas != 0))
  overlap <- sum(aseg_mask & schaefer_mask)

  # Some overlap is expected at boundaries, but should be minimal
  overlap_percent <- overlap / sum(aseg_mask) * 100
  expect_true(overlap_percent < 10)  # Less than 10% overlap

  # Test 2: Create test data and reduce with merged atlas
  test_data <- neuroim2::NeuroVol(
    rnorm(prod(dim(merged$atlas))),
    space = neuroim2::space(merged$atlas)
  )

  reduced <- reduce_atlas(merged, test_data, mean)

  # Should have values for all regions
  expect_equal(nrow(reduced), length(merged$ids))

  # Test 3: Map values back to merged atlas
  test_vals <- rnorm(length(merged$ids))
  mapped <- map_atlas(merged, vals = test_vals, thresh = c(0, 0))

  expect_equal(nrow(mapped), length(merged$ids))

  # Hemisphere information should be preserved from both atlases
  hemi_table <- table(mapped$hemi, useNA = "always")
  expect_true("left" %in% names(hemi_table))
  expect_true("right" %in% names(hemi_table))

  # ASEG might have some bilateral/midline structures
  na_count <- sum(is.na(merged$hemi))
  expect_equal(na_count, sum(is.na(aseg$hemi)))
})
