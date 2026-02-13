test_that("atlas_ref constructor and validator behave as expected", {
  ref <- new_atlas_ref(
    family = "toy",
    model = "ToyModel",
    representation = "volume",
    template_space = "MNI152NLin2009cAsym",
    coord_space = "MNI152",
    confidence = "high"
  )

  expect_s3_class(ref, "atlas_ref")
  expect_equal(ref$family, "toy")
  expect_equal(ref$model, "ToyModel")
  expect_equal(ref$confidence, "high")
  expect_error(validate_atlas_ref(list()), "atlas_ref")
})


test_that("atlas_ref fallback works for legacy atlas objects", {
  legacy <- list(
    name = "LegacyAtlas",
    ids = 1L,
    labels = "ROI1",
    hemi = NA_character_
  )
  class(legacy) <- c("legacy", "atlas")

  ref <- atlas_ref(legacy)
  expect_s3_class(ref, "atlas_ref")
  expect_equal(ref$family, "legacy")
  expect_equal(ref$model, "LegacyAtlas")
  expect_equal(ref$confidence, "uncertain")
})


test_that("bundled atlases expose atlas_ref and compatibility aliases", {
  aseg <- get_aseg_atlas()
  aref <- atlas_ref(aseg)

  expect_s3_class(aref, "atlas_ref")
  expect_equal(atlas_family(aseg), "aseg")
  expect_equal(aseg$template_space, aref$template_space)
  expect_equal(aseg$coord_space, "MNI152")
  expect_equal(aseg$confidence, aref$confidence)

  mtl <- get_olsen_mtl()
  mref <- atlas_ref(mtl)
  expect_equal(atlas_family(mtl), "olsen")
  expect_equal(mtl$template_space, mref$template_space)
  expect_equal(mref$representation, "volume")

  hipp <- get_hipp_atlas(apsections = 2)
  href <- atlas_ref(hipp)
  expect_equal(href$representation, "derived")
  expect_equal(href$family, "olsen")
})


test_that("Schaefer atlas_ref carries expected template metadata", {
  skip_on_cran()

  atl <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7", resolution = "2")
  }, error = function(e) {
    skip("Schaefer download unavailable")
  })

  ref <- atlas_ref(atl)
  expect_equal(ref$family, "schaefer")
  expect_equal(ref$model, "Schaefer2018")
  expect_equal(ref$template_space, "MNI152NLin6Asym")
  expect_equal(ref$coord_space, "MNI152")
})


test_that("Glasser source selection controls provenance/confidence", {
  skip_on_cran()

  atl_2009c <- tryCatch({
    suppressWarnings(get_glasser_atlas(source = "mni2009c"))
  }, error = function(e) {
    skip("Glasser mni2009c source unavailable")
  })

  ref_2009c <- atlas_ref(atl_2009c)
  expect_true(ref_2009c$source %in% c("mni2009c", "xcpengine"))
  if (identical(ref_2009c$source, "mni2009c")) {
    expect_equal(ref_2009c$template_space, "MNI152NLin2009cAsym")
    expect_equal(ref_2009c$confidence, "high")
  } else {
    expect_equal(ref_2009c$template_space, "MNI152_unspecified")
    expect_equal(ref_2009c$confidence, "uncertain")
  }

  atl_legacy <- tryCatch({
    suppressWarnings(get_glasser_atlas(source = "xcpengine"))
  }, error = function(e) {
    skip("Glasser xcpengine source unavailable")
  })
  ref_legacy <- atlas_ref(atl_legacy)
  expect_equal(ref_legacy$template_space, "MNI152_unspecified")
  expect_equal(ref_legacy$confidence, "uncertain")
})


test_that("vol/surf label concordance checks run for Schaefer and Glasser", {
  skip_on_cran()

  schaefer_vol <- tryCatch({
    get_schaefer_atlas(parcels = "100", networks = "7")
  }, error = function(e) {
    skip("Schaefer volume unavailable")
  })
  schaefer_surf <- tryCatch({
    get_schaefer_surfatlas(parcels = "100", networks = "7", surf = "inflated")
  }, error = function(e) {
    skip("Schaefer surface unavailable")
  })

  expect_true(setequal(schaefer_vol$labels, schaefer_surf$labels))

  glasser_vol <- tryCatch({
    suppressWarnings(get_glasser_atlas(source = "mni2009c"))
  }, error = function(e) {
    skip("Glasser volume unavailable")
  })
  glasser_surface <- tryCatch({
    glasser_surf(space = "fsaverage", surf = "pial")
  }, error = function(e) {
    skip("Glasser surface unavailable")
  })

  normalize_glasser_label <- function(x) {
    x <- gsub("^(LH_|RH_|L_|R_|lh_|rh_)", "", x)
    x <- gsub("_ROI$", "", x)
    x
  }

  vol_labels <- unique(normalize_glasser_label(glasser_vol$labels))
  surf_labels <- unique(normalize_glasser_label(glasser_surface$labels))
  overlap <- intersect(vol_labels, surf_labels)
  mismatch_vol <- setdiff(vol_labels, surf_labels)
  mismatch_surf <- setdiff(surf_labels, vol_labels)

  expect_true(length(overlap) > 0)
  expect_true(length(mismatch_vol) < length(vol_labels))
  expect_true(length(mismatch_surf) < length(surf_labels))
})
