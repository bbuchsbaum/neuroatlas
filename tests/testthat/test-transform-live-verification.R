root <- testthat::test_path("..", "..", "data-raw", "transform-artifacts-v1")
skip_if_not(dir.exists(root), "artifact integration scripts are source-only")
source(file.path(root, "scripts", "live-release-checks.R"))

test_that("live verification rejects substituted evidence bytes", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw("original"), path)
  receipt <- list(bytes = file.info(path)$size,
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE))
  expect_true(live_assert_receipt(path, receipt))
  substituted <- receipt
  substituted$sha256 <- strrep("a", 64)
  expect_false(live_receipts_equal(receipt, substituted))
  writeBin(charToRaw("replaced"), path)
  expect_error(live_assert_receipt(path, receipt), "qualified receipt")
})

test_that("a matching disagreement count cannot hide an interior label error", {
  native <- array(c(1, 1, 2, 2), c(2, 2, 1))
  allowed <- array(c(1, 0, 0, 0), dim(native))
  tied <- native
  tied[[1]] <- 2
  expect_identical(live_label_disagreements(tied, native, allowed), 1L)
  interior <- native
  interior[[2]] <- 2
  expect_equal(sum(tied != native), sum(interior != native))
  expect_error(live_label_disagreements(interior, native, allowed),
               "outside the qualified")
})

test_that("native reference geometry is checked independently", {
  skip_if_not_installed("RNifti")
  paths <- c(tempfile(fileext = ".nii.gz"), tempfile(fileext = ".nii.gz"))
  on.exit(unlink(paths), add = TRUE)
  original <- RNifti::asNifti(array(0, c(3, 3, 3)))
  RNifti::writeNifti(original, paths[[1]])
  RNifti::writeNifti(original, paths[[2]])
  expect_true(live_assert_grid(paths[[1]], paths[[2]]))
  shifted <- RNifti::xform(original)
  shifted[1, 4] <- shifted[1, 4] + 2
  attr(shifted, "code") <- 1L
  RNifti::sform(original) <- shifted
  RNifti::qform(original) <- shifted
  RNifti::writeNifti(original, paths[[2]])
  expect_error(live_assert_grid(paths[[1]], paths[[2]]), "geometry differs")
})
