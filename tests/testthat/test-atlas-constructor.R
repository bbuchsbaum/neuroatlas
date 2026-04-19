test_that("new_atlas() assembles a valid atlas object with canonical shape", {
  vol_space <- neuroim2::NeuroSpace(dim = c(4, 4, 4), spacing = c(1, 1, 1))
  arr <- array(0L, dim = c(4, 4, 4))
  arr[1:2, 1:2, 1:2] <- 1L
  arr[3:4, 3:4, 3:4] <- 2L
  vol <- neuroim2::NeuroVol(arr, vol_space)

  ref <- new_atlas_ref(
    family = "toy",
    model = "ToyAtlas",
    representation = "volume",
    template_space = "custom",
    coord_space = "MNI152",
    confidence = "high"
  )

  atl <- new_atlas(
    name = "Toy",
    atlas = vol,
    ids = c(1L, 2L),
    labels = c("RegionA", "RegionB"),
    hemi = c("left", "right"),
    cmap = data.frame(red = c(10, 20), green = c(30, 40), blue = c(50, 60)),
    subclass = "toy",
    ref = ref
  )

  expect_s3_class(atl, "atlas")
  expect_s3_class(atl, "toy")
  expect_equal(atl$name, "Toy")
  expect_equal(atl$ids, c(1L, 2L))
  expect_equal(atl$labels, c("RegionA", "RegionB"))
  expect_equal(atl$orig_labels, c("RegionA", "RegionB"))
  expect_equal(atl$hemi, c("left", "right"))
  expect_true(tibble::is_tibble(atl$roi_metadata))
  expect_equal(atl$roi_metadata$color_r, c(10L, 20L))
  expect_equal(atl$roi_metadata$color_g, c(30L, 40L))
  expect_equal(atl$atlas_ref$family, "toy")
  expect_equal(atl$template_space, "custom")
})

test_that("new_atlas() rejects inconsistent field lengths", {
  vol_space <- neuroim2::NeuroSpace(dim = c(2, 2, 2), spacing = c(1, 1, 1))
  vol <- neuroim2::NeuroVol(array(0L, dim = c(2, 2, 2)), vol_space)
  ref <- new_atlas_ref(
    family = "toy", model = "Toy", representation = "volume",
    template_space = "custom", coord_space = "MNI152", confidence = "high"
  )

  expect_error(
    new_atlas(
      name = "Toy",
      atlas = vol,
      ids = 1:3,
      labels = c("A", "B"),
      ref = ref
    ),
    class = "neuroatlas_error_invalid_atlas"
  )
})

test_that("new_atlas() normalises matrix cmaps to RGB data frames", {
  vol_space <- neuroim2::NeuroSpace(dim = c(2, 2, 2), spacing = c(1, 1, 1))
  vol <- neuroim2::NeuroVol(array(0L, dim = c(2, 2, 2)), vol_space)
  ref <- new_atlas_ref(
    family = "toy", model = "Toy", representation = "volume",
    template_space = "custom", coord_space = "MNI152", confidence = "high"
  )

  m <- matrix(c(11, 12, 21, 22, 31, 32), nrow = 2)  # 2 rows x 3 cols
  atl <- new_atlas(
    name = "Toy",
    atlas = vol,
    ids = 1:2,
    labels = c("A", "B"),
    cmap = m,
    ref = ref
  )

  expect_s3_class(atl$cmap, "data.frame")
  expect_equal(atl$roi_metadata$color_r, c(11L, 12L))
  expect_equal(atl$roi_metadata$color_g, c(21L, 22L))
  expect_equal(atl$roi_metadata$color_b, c(31L, 32L))
})

test_that("atlas registry exposes built-in atlases and resolves aliases", {
  atlases <- list_atlases()
  expect_s3_class(atlases, "tbl_df")
  expect_true(all(c("schaefer", "glasser", "aseg", "olsen_mtl") %in% atlases$id))

  spec <- find_atlas_spec("schaefer2018")
  expect_s3_class(spec, "atlas_spec")
  expect_equal(spec$id, "schaefer")
  expect_equal(spec$loader, "get_schaefer_atlas")

  expect_error(find_atlas_spec("nonsense-atlas"),
               class = "neuroatlas_error_unknown_atlas")
})

test_that(".neuroatlas_download throws neuroatlas_error_download on failure", {
  skip_on_cran()
  # Short timeout so the test doesn't hang on misbehaving networks.
  old_timeout <- getOption("timeout")
  options(timeout = 2)
  on.exit(options(timeout = old_timeout), add = TRUE)

  expect_error(
    .neuroatlas_download(
      url = "http://127.0.0.1:0/definitely-not-here.bin",
      dest = tempfile(fileext = ".bin"),
      description = "unit-test asset"
    ),
    class = "neuroatlas_error_download"
  )
})

test_that(".neuroatlas_try_download captures error instead of swallowing it", {
  skip_on_cran()
  old_timeout <- getOption("timeout")
  options(timeout = 2)
  on.exit(options(timeout = old_timeout), add = TRUE)

  status <- .neuroatlas_try_download(
    url = "http://127.0.0.1:0/definitely-not-here.bin",
    dest = tempfile(fileext = ".bin"),
    description = "unit-test asset"
  )
  expect_false(status$ok)
  expect_null(status$path)
  expect_true(inherits(status$error, "condition"))
})

test_that(".is_lfs_pointer detects pointer stubs", {
  f <- tempfile(fileext = ".ptr")
  writeLines("version https://git-lfs.github.com/spec/v1", f)
  expect_true(.is_lfs_pointer(f))

  f2 <- tempfile(fileext = ".bin")
  writeLines(paste(rep("x", 2048), collapse = ""), f2)
  expect_false(.is_lfs_pointer(f2))
})
