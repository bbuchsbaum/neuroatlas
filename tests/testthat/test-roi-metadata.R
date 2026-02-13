# Tests for ROI metadata functions

test_that("roi_metadata returns tibble with expected columns for ASEG atlas", {
  skip_on_cran()

  atlas <- get_aseg_atlas()
  meta <- roi_metadata(atlas)

  expect_s3_class(meta, "tbl_df")
  expect_true(all(c("id", "label", "label_full", "hemi") %in% names(meta)))
  expect_equal(nrow(meta), length(atlas$ids))
  expect_equal(meta$id, atlas$ids)
  expect_equal(meta$label, atlas$labels)
  expect_equal(meta$hemi, atlas$hemi)
})

test_that("roi_metadata returns tibble with network for Schaefer atlas", {
  skip_on_cran()

  atlas <- get_schaefer_atlas(parcels = "100", networks = "7")
  meta <- roi_metadata(atlas)

  expect_s3_class(meta, "tbl_df")
  expect_true("network" %in% names(meta))
  expect_equal(nrow(meta), length(atlas$ids))
  expect_false(all(is.na(meta$network)))
})

test_that("roi_attributes returns available attributes", {
  skip_on_cran()

  # ASEG atlas - no network

  aseg <- get_aseg_atlas()
  attrs <- roi_attributes(aseg)

  expect_type(attrs, "character")
  expect_true(all(c("label", "label_full", "hemi") %in% attrs))
  # ASEG doesn't have network, so it shouldn't appear
  # (network is NULL, so should be filtered out)

  # Schaefer atlas - has network
  schaefer <- get_schaefer_atlas(parcels = "100", networks = "7")
  attrs_s <- roi_attributes(schaefer)

  expect_true("network" %in% attrs_s)
})

test_that("filter_atlas filters by hemisphere", {
  skip_on_cran()

  atlas <- get_aseg_atlas()
  left_atlas <- filter_atlas(atlas, hemi == "left")

  expect_s3_class(left_atlas, "atlas")
  expect_true(all(left_atlas$hemi == "left"))
  expect_lt(length(left_atlas$ids), length(atlas$ids))

  # Verify roi_metadata is also filtered
  meta <- roi_metadata(left_atlas)
  expect_true(all(meta$hemi == "left"))
})

test_that("filter_atlas filters by network (Schaefer)", {
  skip_on_cran()

  atlas <- get_schaefer_atlas(parcels = "100", networks = "7")
  vis_atlas <- filter_atlas(atlas, network == "Vis")

  expect_s3_class(vis_atlas, "atlas")
  expect_true(all(vis_atlas$network == "Vis"))
  expect_lt(length(vis_atlas$ids), length(atlas$ids))
})

test_that("filter_atlas supports combined filters", {
  skip_on_cran()

  atlas <- get_schaefer_atlas(parcels = "100", networks = "7")
  left_vis <- filter_atlas(atlas, hemi == "left", network == "Vis")

  expect_s3_class(left_vis, "atlas")
  expect_true(all(left_vis$hemi == "left"))
  expect_true(all(left_vis$network == "Vis"))
})

test_that("filter_atlas errors on empty result", {
  skip_on_cran()

  atlas <- get_aseg_atlas()
  expect_error(filter_atlas(atlas, hemi == "nonexistent"))
})

test_that("get_roi with hemi parameter works", {
  skip_on_cran()

  atlas <- get_aseg_atlas()

  # Get left hippocampus only
  left_hippo <- get_roi(atlas, label = "Hippocampus", hemi = "left")
  expect_length(left_hippo, 1)

  # Get both hemispheres
  both_hippo <- get_roi(atlas, label = "Hippocampus")
  expect_length(both_hippo, 1)

  # Left should have fewer voxels than both
  # (or same if label only occurs once in left)
  left_coords <- left_hippo[[1]]@coords
  both_coords <- both_hippo[[1]]@coords
  expect_lte(nrow(left_coords), nrow(both_coords))
})

test_that("get_roi errors for invalid hemisphere", {
  skip_on_cran()

  atlas <- get_aseg_atlas()
  expect_error(get_roi(atlas, label = "Hippocampus", hemi = "invalid"))
})

test_that("get_roi errors when label not found in specified hemisphere", {
  skip_on_cran()

  atlas <- get_aseg_atlas()
  # Brainstem has hemi = NA, not "left"
  expect_error(get_roi(atlas, label = "Brainstem", hemi = "left"),
               "not found.*hemisphere")
})

test_that("roi_metadata is backwards compatible with legacy atlases", {
  skip_on_cran()

  # Create a minimal legacy atlas object without roi_metadata field
  legacy_atlas <- list(
    name = "test_atlas",
    atlas = array(c(0, 1, 2, 0, 1, 2), dim = c(2, 3, 1)),
    ids = c(1L, 2L),
    labels = c("region_A", "region_B"),
    orig_labels = c("left_region_A", "right_region_B"),
    hemi = c("left", "right"),
    cmap = data.frame(red = c(255, 0), green = c(0, 255), blue = c(0, 0)),
    network = NULL
  )
  class(legacy_atlas) <- "atlas"

  # roi_metadata should construct tibble from legacy fields
  meta <- roi_metadata(legacy_atlas)
  expect_s3_class(meta, "tbl_df")
  expect_equal(meta$id, c(1L, 2L))
  expect_equal(meta$label, c("region_A", "region_B"))
  expect_equal(meta$label_full, c("left_region_A", "right_region_B"))
  expect_equal(meta$hemi, c("left", "right"))
})

test_that("filter_atlas preserves atlas class", {
  skip_on_cran()

  atlas <- get_aseg_atlas()
  filtered <- filter_atlas(atlas, hemi == "left")

  # Should preserve original class hierarchy
  expect_true("aseg" %in% class(filtered))
  expect_true("atlas" %in% class(filtered))
})

test_that("roi_metadata color columns are integers", {
  skip_on_cran()

  atlas <- get_schaefer_atlas(parcels = "100", networks = "7")
  meta <- roi_metadata(atlas)

  expect_type(meta$color_r, "integer")
  expect_type(meta$color_g, "integer")
  expect_type(meta$color_b, "integer")
})

test_that("roi_metadata.atlas includes atlas_ref columns when available", {
  # Create a mock atlas with atlas_ref
  ref <- new_atlas_ref(
    family = "test",
    model = "TestAtlas",
    representation = "volume",
    template_space = "MNI152NLin6Asym",
    coord_space = "MNI152",
    confidence = "high"
  )

  mock_atlas <- list(
    ids = 1:3,
    labels = c("region_a", "region_b", "region_c"),
    orig_labels = c("Region_A", "Region_B", "Region_C"),
    hemi = c("left", "left", "right"),
    cmap = data.frame(r = c(255, 0, 0), g = c(0, 255, 0), b = c(0, 0, 255)),
    network = NULL,
    atlas_ref = ref
  )
  class(mock_atlas) <- "atlas"

  meta <- roi_metadata(mock_atlas)

  expect_true("template_space" %in% names(meta))
  expect_true("coord_space" %in% names(meta))
  expect_true("atlas_family" %in% names(meta))
  expect_equal(unique(meta$template_space), "MNI152NLin6Asym")
  expect_equal(unique(meta$coord_space), "MNI152")
  expect_equal(unique(meta$atlas_family), "test")
})

test_that("roi_metadata.atlas works without atlas_ref", {
  mock_atlas <- list(
    ids = 1:2,
    labels = c("a", "b"),
    orig_labels = c("A", "B"),
    hemi = c("left", "right"),
    cmap = data.frame(r = c(255, 0), g = c(0, 255), b = c(0, 0)),
    network = NULL
  )
  class(mock_atlas) <- "atlas"

  meta <- roi_metadata(mock_atlas)

  # Should NOT have atlas_ref columns
  expect_false("template_space" %in% names(meta))
  expect_false("coord_space" %in% names(meta))
  expect_false("atlas_family" %in% names(meta))
})
