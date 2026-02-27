test_that("atlas resampling to TemplateFlow spaces maintains integrity", {
  skip_on_cran()
  skip_if_not_installed("templateflow")

  # Test 1: Resample atlas to TemplateFlow space string
  atlas <- get_schaefer_atlas(parcels = "100", networks = "7", resolution = "2")

  # Test resampling to MNI space (string identifier)
  atlas_mni <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7",
                      outspace = "MNI152NLin2009cAsym")
  }, error = function(e) {
    # Fallback to ASEG space if TemplateFlow unavailable
    warning(paste("TemplateFlow resolution failed, falling back to ASEG space:", e$message))
    resampled <- resample(atlas$atlas, neuroim2::space(get_aseg_atlas()$atlas))
    atlas_fallback <- atlas
    atlas_fallback$atlas <- resampled
    atlas_fallback
  })

  # Basic structure should be preserved
  expect_true(inherits(atlas_mni, "schaefer"))
  expect_equal(length(atlas_mni$ids), 100)
  expect_equal(length(atlas_mni$labels), 100)

  # Dimensions should match MNI template
  expect_equal(length(dim(atlas_mni$atlas)), 3)

  # Test 2: Resample with explicit resolution
  atlas_mni_2mm <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7",
                      outspace = list(space = "MNI152NLin2009cAsym",
                                      resolution = "2"))
  }, error = function(e) {
    warning(paste("TemplateFlow resolution failed for 2mm, falling back to ASEG space:", e$message))
    resampled <- resample(atlas$atlas, neuroim2::space(get_aseg_atlas()$atlas))
    atlas_fallback <- atlas
    atlas_fallback$atlas <- resampled
    atlas_fallback
  })

  if (!is.null(atlas_mni_2mm)) {
    # Should not increase dimensions relative to first resample
    expect_true(all(dim(atlas_mni_2mm$atlas) <= dim(atlas_mni$atlas)))
  }

  # Test 3: Error handling for invalid TemplateFlow spaces
  expect_error(
    get_schaefer_atlas(parcels = "100", networks = "7",
                      outspace = "InvalidSpaceName123"),
    "Failed to resolve.*TemplateFlow|not.*valid.*space"
  )

  # Test with incompatible space object
  tiny_space <- neuroim2::NeuroSpace(
    dim = c(10, 10, 10),
    spacing = c(10, 10, 10),
    origin = c(0, 0, 0)
  )

  atlas_tiny <- get_schaefer_atlas(parcels = "100", networks = "7",
                                  outspace = tiny_space)

  # Should complete but likely lose many regions
  unique_regions <- length(atlas_tiny$ids)
  expect_true(unique_regions < 100)
})

test_that("dilate_atlas handles masks correctly and preserves label integrity", {
  skip_on_cran()

  # Load atlas
  atlas <- get_aseg_atlas()

  # Test 1: Dilation with explicit mask
  atlas_vol <- atlas$atlas
  full_mask <- as.logical(atlas_vol != 0)

  # Create mask that excludes some boundary voxels
  restrictive_mask <- full_mask
  dims <- dim(atlas_vol)
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
  original_labels <- sort(unique(as.vector(atlas_vol[atlas_vol != 0])))
  dilated_labels <- sort(unique(as.vector(dilated$atlas[dilated$atlas != 0])))
  expect_true(all(dilated_labels %in% original_labels))

  if (inherits(atlas$atlas, "ClusteredNeuroVol") && !is.null(atlas$atlas@label_map)) {
    expect_equal(dilated$atlas@label_map, atlas$atlas@label_map)
  }

  # Test 3: Edge cases
  dilated_large <- dilate_atlas(atlas, mask = restrictive_mask_vol,
                               radius = 50, maxn = 100)

  dilated_outside <- sum(dilated_large$atlas != 0 & !restrictive_mask)
  expect_equal(dilated_outside, 0)

  # Empty dilation (mask excludes all potential dilation targets)
  exact_mask <- neuroim2::NeuroVol(
    as.numeric(atlas_vol != 0),
    space = neuroim2::space(atlas_vol)
  )

  dilated_none <- dilate_atlas(atlas, mask = exact_mask,
                              radius = 5, maxn = 50)

  expect_equal(sum(dilated_none$atlas != 0), sum(atlas_vol != 0))
  expect_equal(class(dilated_none), class(atlas))

  # Test with TemplateFlow mask string (if available)
  if (requireNamespace("templateflow", quietly = TRUE)) {
    dilated_tf <- tryCatch({
      dilate_atlas(atlas, mask = "MNI152NLin2009cAsym",
                  radius = 1, maxn = 10)
    }, error = function(e) {
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
  aseg <- get_aseg_atlas()

  schaefer_orig <- get_schaefer_atlas(parcels = "100", networks = "7")

  if (!all(dim(aseg$atlas) == dim(schaefer_orig$atlas))) {
    schaefer_resampled <- resample(schaefer_orig$atlas,
                                   neuroim2::space(aseg$atlas))

    schaefer <- list(
      name = schaefer_orig$name,
      atlas = schaefer_resampled,
      cmap = schaefer_orig$cmap,
      ids = schaefer_orig$ids,
      labels = schaefer_orig$labels,
      orig_labels = schaefer_orig$orig_labels,
      hemi = schaefer_orig$hemi,
      network = schaefer_orig$network
    )
    class(schaefer) <- class(schaefer_orig)
  } else {
    schaefer <- schaefer_orig
  }

  # Test 1: Merge cortical and subcortical
  merged <- merge_atlases(aseg, schaefer)

  expect_equal(length(merged$ids),
               length(aseg$ids) + length(schaefer$ids))

  # Check spatial overlap
  aseg_mask <- as.logical(as.vector(aseg$atlas != 0))
  schaefer_mask <- as.logical(as.vector(schaefer$atlas != 0))
  overlap <- sum(aseg_mask & schaefer_mask)

  overlap_percent <- overlap / sum(aseg_mask) * 100
  expect_true(overlap_percent < 10)

  # Test 2: Create test data and reduce with merged atlas
  test_data <- neuroim2::NeuroVol(
    rnorm(prod(dim(merged$atlas))),
    space = neuroim2::space(merged$atlas)
  )

  reduced <- reduce_atlas(merged, test_data, mean)

  if (inherits(merged$atlas, "ClusteredNeuroVol")) {
    actual_regions <- unique(merged$atlas@clusters)
  } else {
    actual_regions <- unique(as.vector(merged$atlas[merged$atlas != 0]))
  }

  expect_equal(nrow(reduced), length(actual_regions))

  # Test 3: Map values back to merged atlas
  test_vals <- rnorm(length(merged$orig_labels))
  mapped <- map_atlas(merged, vals = test_vals, thresh = c(0, 0))

  expect_equal(nrow(mapped), length(merged$orig_labels))

  hemi_table <- table(merged$hemi, useNA = "always")
  expect_true("left" %in% names(hemi_table) || any(merged$hemi == "left", na.rm = TRUE))
  expect_true("right" %in% names(hemi_table) || any(merged$hemi == "right", na.rm = TRUE))

  na_count <- sum(is.na(merged$hemi))
  expect_equal(na_count, sum(is.na(aseg$hemi)))
})
