wang_or_skip <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      testthat::skip(paste("Wang atlas / TemplateFlow unavailable:",
                           conditionMessage(e)))
    }
  )
}

test_that("Wang ROI table lists the 25 topographic areas", {
  tbl <- neuroatlas:::.wang_visual_labels()
  expect_equal(nrow(tbl), 25L)
  expect_equal(tbl$id, seq_len(25L))
  expect_true(all(c("V1v", "V1d", "V2v", "V2d", "V3v", "V3d", "hV4",
                    "VO1", "LO1", "TO1", "V3A", "IPS0", "FEF") %in% tbl$label))
})

test_that(".read_mgh_data round-trips a minimal MGH overlay", {
  # Build a minimal uncompressed MGH file: 5 int32 values [10,20,30,40,50].
  path <- tempfile(fileext = ".mgh")
  con <- file(path, "wb")
  # header: version, width, height, depth, nframes, type(=1 int), dof
  writeBin(as.integer(c(1L, 5L, 1L, 1L, 1L, 1L, 0L)), con, size = 4L,
           endian = "big")
  # pad to byte 284 (28 already written -> 256 bytes of zero padding)
  writeBin(raw(284L - 28L), con)
  writeBin(as.integer(c(10L, 20L, 30L, 40L, 50L)), con, size = 4L,
           endian = "big")
  close(con)

  vals <- neuroatlas:::.read_mgh_data(path)
  expect_equal(length(vals), 5L)
  expect_equal(as.integer(vals), c(10L, 20L, 30L, 40L, 50L))
})

test_that("Wang atlas is registered for get_atlas dispatch", {
  specs <- list_atlases()
  expect_true("wang" %in% specs$id)
  expect_equal(specs$representation[specs$id == "wang"], "surface")
  expect_s3_class(find_atlas_spec("wang2015"), "atlas_spec")
})

test_that("get_wang_prob_atlas returns a manifest without downloading", {
  man <- get_wang_prob_atlas(image = "probability", hemi = "both")

  expect_s3_class(man, "wang_prob_paths")
  expect_equal(man$image, "probability")
  expect_equal(nrow(man$files), 50L) # 25 areas x 2 hemispheres
  expect_false(any(man$files$exists)) # nothing resolved without prob_dir
  expect_true(all(grepl("perc_VTPM_vol_roi", man$files$member)))
  expect_true(any(grepl("perc_VTPM_vol_roi7_lh", man$files$member))) # hV4 = roi 7
  expect_match(man$zip_url, "ProbAtlas_v4\\.zip$")

  mp <- get_wang_prob_atlas(image = "maxprob", hemi = "lh")
  expect_equal(nrow(mp$files), 1L)
  expect_match(mp$files$member, "maxprob_vol_lh\\.nii\\.gz$")

  # ROI subsetting by label and by id.
  sub_lab <- get_wang_prob_atlas(rois = c("V1v", "hV4"), hemi = "rh")
  expect_equal(nrow(sub_lab$files), 2L)
  expect_setequal(sub_lab$files$label, c("V1v", "hV4"))
  expect_error(get_wang_prob_atlas(rois = "NotAnArea"),
               class = "neuroatlas_error_wang_prob")
})

test_that("get_wang_prob_atlas resolves and loads from a local prob_dir", {
  # Synthesize a minimal ProbAtlas_v4/subj_vol_all with two maxprob volumes.
  root <- tempfile("ProbAtlas_v4-")
  svd <- file.path(root, "subj_vol_all")
  dir.create(svd, recursive = TRUE)
  sp <- neuroim2::NeuroSpace(c(4L, 4L, 4L), spacing = c(2, 2, 2))
  for (h in c("lh", "rh")) {
    vol <- neuroim2::NeuroVol(array(runif(64), c(4, 4, 4)), sp)
    neuroim2::write_vol(vol, file.path(svd, paste0("maxprob_vol_", h, ".nii.gz")))
  }

  man <- get_wang_prob_atlas(prob_dir = root, image = "maxprob", hemi = "both")
  expect_true(all(man$files$exists))

  loaded <- get_wang_prob_atlas(prob_dir = root, image = "maxprob",
                                hemi = "both", path_only = FALSE)
  expect_s3_class(loaded, "wang_prob_volumes")
  expect_named(loaded$volumes, c("lh", "rh"))
  expect_s4_class(loaded$volumes[["lh"]], "NeuroVol")

  # Missing probability volumes -> informative error in load mode.
  expect_error(
    get_wang_prob_atlas(prob_dir = root, image = "probability",
                        path_only = FALSE),
    class = "neuroatlas_error_wang_prob"
  )
})

test_that("get_wang_atlas loads an fsaverage surface atlas", {
  skip_on_cran()

  atlas <- wang_or_skip(get_wang_atlas(surf = "inflated"))

  expect_s3_class(atlas, "wang")
  expect_s3_class(atlas, "surfatlas")
  expect_equal(atlas$surface_space, "fsaverage")
  expect_s4_class(atlas$lh_atlas, "LabeledNeuroSurface")
  expect_s4_class(atlas$rh_atlas, "LabeledNeuroSurface")
  expect_equal(length(atlas$ids), 50L) # 25 areas x 2 hemispheres
  expect_true("hV4" %in% atlas$labels)
  expect_true(all(c("left", "right") %in% atlas$hemi))
  expect_equal(atlas_ref(atlas)$family, "wang")
})
