make_mock_atlas <- function(ref, name = "mock") {
  x <- list(
    name = name,
    ids = 1L,
    labels = "ROI1",
    orig_labels = "ROI1",
    hemi = NA_character_,
    atlas_ref = ref
  )
  class(x) <- c(ref$family, "atlas")
  x
}

test_that("atlas_alignment reports exact identity for identical references", {
  ref <- new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "MNI152NLin6Asym",
    coord_space = "MNI152",
    confidence = "high"
  )
  a <- make_mock_atlas(ref)
  b <- make_mock_atlas(ref)

  aln <- atlas_alignment(a, b)
  expect_s3_class(aln, "atlas_alignment")
  expect_true(aln$compatible)
  expect_equal(aln$relation, "identical")
  expect_equal(aln$confidence, "exact")
  expect_false(aln$requires_transform)
})


test_that("atlas_alignment detects same representation across templates", {
  a <- make_mock_atlas(new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "MNI152NLin6Asym",
    coord_space = "MNI152",
    confidence = "high"
  ))
  b <- make_mock_atlas(new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "MNI152NLin2009cAsym",
    coord_space = "MNI152",
    confidence = "high"
  ))

  aln <- atlas_alignment(a, b)
  expect_true(aln$compatible)
  expect_equal(aln$relation, "same_representation_different_template")
  expect_equal(aln$method, "nonlinear_warp")
  expect_equal(aln$confidence, "high")
  expect_equal(aln$status, "planned")
  expect_true(aln$requires_transform)
})


test_that("atlas_alignment falls back when no template route is registered", {
  a <- make_mock_atlas(new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "TemplateX",
    coord_space = "MNI152",
    confidence = "high"
  ))
  b <- make_mock_atlas(new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "TemplateY",
    coord_space = "MNI152",
    confidence = "high"
  ))

  aln <- atlas_alignment(a, b)
  expect_true(aln$compatible)
  expect_equal(aln$relation, "same_representation_different_template")
  expect_equal(aln$method, "resample")
  expect_equal(aln$confidence, "approximate")
  expect_equal(aln$status, "planned")
  expect_match(aln$notes, "no direct route registered", ignore.case = TRUE)
})


test_that("atlas_alignment normalizes template IDs before lookup", {
  a <- make_mock_atlas(new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "mni152nlin6asym",
    coord_space = "MNI152",
    confidence = "high"
  ))
  b <- make_mock_atlas(new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "MNI152NLin2009cAsym",
    coord_space = "MNI152",
    confidence = "high"
  ))

  aln <- atlas_alignment(a, b)
  expect_equal(aln$method, "nonlinear_warp")
  expect_equal(aln$confidence, "high")
})


test_that("atlas_alignment uses registry for cross-representation routes", {
  vol <- make_mock_atlas(new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "MNI152NLin6Asym",
    coord_space = "MNI152",
    confidence = "high"
  ))
  surf <- make_mock_atlas(new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "surface",
    template_space = "fsaverage6",
    coord_space = "MNI305",
    confidence = "high"
  ))

  aln <- atlas_alignment(vol, surf)
  expect_true(aln$compatible)
  expect_equal(aln$relation, "same_model_projected")
  expect_equal(aln$status, "planned")
})


test_that("atlas_alignment returns unknown for unregistered routes", {
  aseg <- get_aseg_atlas()
  olsen <- get_olsen_mtl()

  aln <- atlas_alignment(aseg, olsen)
  expect_false(aln$compatible)
  expect_equal(aln$relation, "unknown")
  expect_equal(aln$confidence, "uncertain")
  expect_equal(aln$status, "planned")
})


test_that("atlas_transform_manifest exposes route table", {
  manifest <- atlas_transform_manifest()
  expect_true(is.data.frame(manifest))
  expect_true(all(c(
    "family", "model", "from_representation", "to_representation",
    "relation", "method", "confidence", "status", "notes"
  ) %in% names(manifest)))
  expect_true(all(manifest$status %in% c("available", "planned")))
})


test_that("print.atlas_alignment returns invisibly", {
  ref <- new_atlas_ref(
    family = "schaefer",
    model = "Schaefer2018",
    representation = "volume",
    template_space = "MNI152NLin6Asym",
    coord_space = "MNI152",
    confidence = "high"
  )
  aln <- atlas_alignment(make_mock_atlas(ref), make_mock_atlas(ref))
  expect_invisible(print(aln))
})
