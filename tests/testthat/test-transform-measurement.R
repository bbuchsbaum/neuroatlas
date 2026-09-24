artifact_root <- testthat::test_path("..", "..", "data-raw", "transform-artifacts-v1")
testthat::skip_if_not(dir.exists(artifact_root), "artifact build scripts are source-repository only")
artifact_root <- normalizePath(artifact_root)
withr::local_envvar(c(NEUROATLAS_TRANSFORM_ARTIFACT_ROOT = artifact_root))
source(file.path(artifact_root, "scripts", "measure-qualification.R"))

write_tiny_composite_h5 <- function(path, affine_translation = 0,
                                    legacy_names = FALSE) {
  h5 <- hdf5r::H5File$new(path, mode = "w")
  on.exit(h5$close_all(), add = TRUE)
  transforms <- h5$create_group("TransformGroup")
  warp <- transforms$create_group("0")
  warp[["TransformType"]] <- "DisplacementFieldTransform_double_3_3"
  fixed_name <- if (legacy_names) "TranformFixedParameters" else "TransformFixedParameters"
  parameter_name <- if (legacy_names) "TranformParameters" else "TransformParameters"
  warp[[fixed_name]] <- c(3, 3, 3, 0, 0, 0, 1, 1, 1,
    1, 0, 0, 0, 1, 0, 0, 0, 1)
  warp[[parameter_name]] <- rep(0, 81)
  affine <- transforms$create_group("1")
  affine[["TransformType"]] <- "AffineTransform_double_3_3"
  affine[["TransformParameters"]] <- c(1, 0, 0, 0, 1, 0, 0, 0, 1,
    affine_translation, 0, 0)
  affine[["TransformFixedParameters"]] <- c(0, 0, 0)
}

test_that("support applies a preceding affine before testing a warp grid", {
  path <- tempfile(fileext = ".h5")
  write_tiny_composite_h5(path, affine_translation = 10)
  support <- measure_warp_support(path, matrix(c(0, 0, 0), ncol = 3))
  expect_false(support$supported[[1L]])
  expect_true(support$available)
})

test_that("legacy H5 fields and field faces are decoded independently", {
  path <- tempfile(fileext = ".h5")
  write_tiny_composite_h5(path, legacy_names = TRUE)
  support <- measure_warp_support(path,
    rbind(c(-1, -1, 1), c(0, -1, 1), c(-3, -1, 1)))
  expect_equal(support$supported, c(TRUE, TRUE, FALSE))
  expect_equal(support$boundary, c(FALSE, TRUE, FALSE))
})

test_that("identity and affine-only support every finite probe", {
  points <- matrix(c(0, 0, 0, 100, -20, 4), ncol = 3, byrow = TRUE)
  support <- measure_warp_support("unused", points, identity = TRUE)
  expect_true(all(support$supported))
  expect_false(any(support$boundary))
})

test_that("label Dice includes missing frozen source IDs as zero", {
  fixed <- array(c(1, 1, 0, 0), dim = c(2, 2, 1))
  moved <- array(c(1, 1, 0, 0), dim = c(2, 2, 1))
  dice <- measure_dice(fixed, moved, source_ids = c(0L, 1L, 2L))
  expect_equal(dice$min, 0)
  expect_equal(unname(dice$per_label[["2"]]), 0)
  expect_equal(dice$missing_from_warp, 2L)
  json <- jsonlite::fromJSON(jsonlite::toJSON(dice, auto_unbox = TRUE),
                            simplifyVector = FALSE)
  expect_equal(json$per_label[["2"]], 0)
})

test_that("image round trips retain typed diagnostics", {
  root <- tempfile("roundtrip-"); dir.create(root)
  write_image <- function(name, values) {
    path <- file.path(root, name)
    RNifti::writeNifti(values, path)
    path
  }
  mask <- array(c(1, 1, 0, 0, 0, 0, 0, 0), c(2, 2, 2))
  labels <- array(c(1, 2, 0, 0, 0, 0, 0, 0), c(2, 2, 2))
  original_mask <- write_image("mask.nii.gz", mask)
  original_labels <- write_image("labels.nii.gz", labels)
  perfect <- measure_image_roundtrip(original_mask, original_labels,
    write_image("mask-back.nii.gz", mask), write_image("labels-back.nii.gz", labels),
    original_mask)
  expect_equal(perfect$mask_dice, 1)
  expect_true(perfect$geometry_exact)
  expect_true(perfect$finite)
  lost <- measure_image_roundtrip(original_mask, original_labels,
    write_image("lost-mask.nii.gz", mask), write_image("lost-labels.nii.gz", replace(labels, 2, 0)), original_mask)
  expect_equal(lost$label_dice$per_label[["2"]], 0)
  expect_equal(lost$lost_ids, 2L)
  fractional <- measure_image_roundtrip(original_mask, original_labels,
    write_image("fraction-mask.nii.gz", mask), write_image("fraction-labels.nii.gz", replace(labels, 1, 1.5)), original_mask)
  expect_false(fractional$integer_labels)
  expect_false(fractional$allowed_label_ids)
  expect_true(is.na(fractional$label_dice$min))
  expect_true(is.na(fractional$lost_ids))
  bad_mask <- mask; bad_mask[[1]] <- Inf
  nonfinite <- measure_image_roundtrip(original_mask, original_labels,
    write_image("nonfinite-mask.nii.gz", bad_mask), write_image("nonfinite-labels.nii.gz", labels), original_mask)
  expect_false(nonfinite$finite)
  expect_gt(nonfinite$nonfinite_mask, 0)
  mismatch <- measure_image_roundtrip(original_mask, original_labels,
    write_image("small-mask.nii.gz", array(0, c(1, 1, 1))), write_image("small-labels.nii.gz", array(0, c(1, 1, 1))), original_mask)
  expect_false(mismatch$geometry_exact)
  expect_true(is.na(mismatch$mask_dice))
  bad_labels <- measure_image_roundtrip(original_mask, original_labels,
    original_mask, write_image("nan-labels.nii.gz", replace(labels, 1, NaN)),
    original_mask)
  expect_false(bad_labels$finite)
  expect_gt(bad_labels$nonfinite_labels, 0)
  expect_true(is.na(bad_labels$label_dice$min))
  novel <- measure_image_roundtrip(original_mask, original_labels,
    original_mask, write_image("new-labels.nii.gz", replace(labels, 1, 77)),
    original_mask)
  expect_true(novel$integer_labels)
  expect_false(novel$allowed_label_ids)
})

test_that("geometry compares physical values independently of NIfTI intent tags", {
  skip_if_not_installed("RNifti")
  a <- RNifti::asNifti(array(0, c(3, 3, 3)))
  matrix <- diag(4)
  attr(matrix, "code") <- 1L
  RNifti::sform(a) <- matrix
  RNifti::qform(a) <- matrix
  b <- RNifti::asNifti(array(0, c(3, 3, 3)))
  attr(matrix, "code") <- 4L
  RNifti::sform(b) <- matrix
  RNifti::qform(b) <- matrix
  source <- tempfile(fileext = ".nii.gz")
  target <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(a, source)
  RNifti::writeNifti(b, target)
  expect_true(measure_geometry_exact(source, target))
  matrix <- RNifti::xform(b)
  matrix[1, 4] <- matrix[1, 4] + 1
  RNifti::sform(b) <- matrix
  RNifti::qform(b) <- matrix
  RNifti::writeNifti(b, target)
  expect_false(measure_geometry_exact(source, target))
})
