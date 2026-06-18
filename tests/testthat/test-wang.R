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

test_that("get_wang_prob_atlas builds a manifest without downloading", {
  # Empty redirected cache: a manifest must not download, so nothing resolves.
  cache_root <- tempfile("wang-cache-")
  dir.create(cache_root, recursive = TRUE)
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit({
    if (is.na(old_cache)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache)
    }
  }, add = TRUE)

  man <- get_wang_prob_atlas(image = "probability", hemi = "both")
  expect_s3_class(man, "wang_prob_paths")
  expect_equal(man$image, "probability")
  expect_equal(nrow(man$files), 50L) # 25 areas x 2 hemispheres
  expect_false(any(man$files$exists)) # not downloaded by a manifest call
  expect_true(all(grepl("perc_VTPM_vol_roi", man$files$member)))
  expect_true(any(grepl("perc_VTPM_vol_roi7_lh", man$files$member))) # hV4 = roi 7
  expect_match(man$resources_url, "scholar\\.princeton\\.edu")
  expect_match(man$download_url, "releases/download/wang-probatlas-v4")

  mp <- get_wang_prob_atlas(image = "maxprob", hemi = "lh")
  expect_equal(nrow(mp$files), 1L)
  expect_match(mp$files$member, "maxprob_vol_lh\\.nii\\.gz$")

  # ROI subsetting by label and by id (manifest structure only).
  sub_lab <- get_wang_prob_atlas(rois = c("V1v", "hV4"), hemi = "rh")
  expect_equal(nrow(sub_lab$files), 2L)
  expect_setequal(sub_lab$files$label, c("V1v", "hV4"))
  expect_error(get_wang_prob_atlas(rois = "NotAnArea"),
               class = "neuroatlas_error_wang_prob")
})

test_that("get_wang_prob_atlas resolves and loads from a local prob_dir", {
  # Keep cache writes out of the real user cache during the test.
  cache_root <- tempfile("wang-cache-")
  dir.create(cache_root, recursive = TRUE)
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit({
    if (is.na(old_cache)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache)
    }
  }, add = TRUE)

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

test_that("get_wang_prob_atlas seeds the cache so later calls need no prob_dir", {
  # Redirect the neuroatlas cache into a temp dir for the test.
  cache_root <- tempfile("wang-cache-")
  dir.create(cache_root, recursive = TRUE)
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit({
    if (is.na(old_cache)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache)
    }
  }, add = TRUE)

  # Synthesize a user-supplied ProbAtlas_v4 directory with maxprob volumes.
  root <- tempfile("ProbAtlas_v4-")
  svd <- file.path(root, "subj_vol_all")
  dir.create(svd, recursive = TRUE)
  sp <- neuroim2::NeuroSpace(c(4L, 4L, 4L), spacing = c(2, 2, 2))
  for (h in c("lh", "rh")) {
    vol <- neuroim2::NeuroVol(array(runif(64), c(4, 4, 4)), sp)
    neuroim2::write_vol(vol, file.path(svd, paste0("maxprob_vol_", h, ".nii.gz")))
  }

  # Loading from prob_dir should copy the volumes into the neuroatlas cache.
  loaded <- get_wang_prob_atlas(prob_dir = root, image = "maxprob",
                                hemi = "both", path_only = FALSE)
  expect_s3_class(loaded, "wang_prob_volumes")

  cache_svd <- file.path(.neuroatlas_cache_dir("wang"), "subj_vol_all")
  expect_true(file.exists(file.path(cache_svd, "maxprob_vol_lh.nii.gz")))
  expect_true(file.exists(file.path(cache_svd, "maxprob_vol_rh.nii.gz")))

  # Subsequent calls resolve from the cache without prob_dir.
  man <- get_wang_prob_atlas(image = "maxprob", hemi = "both")
  expect_true(all(man$files$exists))

  loaded2 <- get_wang_prob_atlas(image = "maxprob", hemi = "both",
                                 path_only = FALSE)
  expect_named(loaded2$volumes, c("lh", "rh"))
  expect_s4_class(loaded2$volumes[["lh"]], "NeuroVol")

  # path_only = TRUE must not seed the cache (no side effects on a manifest).
  root2 <- tempfile("ProbAtlas_v4b-")
  svd2 <- file.path(root2, "subj_vol_all")
  dir.create(svd2, recursive = TRUE)
  neuroim2::write_vol(
    neuroim2::NeuroVol(array(runif(64), c(4, 4, 4)), sp),
    file.path(svd2, "perc_VTPM_vol_roi1_lh.nii.gz")
  )
  get_wang_prob_atlas(prob_dir = root2, image = "probability",
                      hemi = "lh", rois = "V1v", path_only = TRUE)
  expect_false(file.exists(file.path(cache_svd, "perc_VTPM_vol_roi1_lh.nii.gz")))
})

test_that("an invalid prob_dir errors instead of falling back to the bundle", {
  # An explicit prob_dir that does not resolve must not silently use cache/bundle.
  expect_error(
    get_wang_prob_atlas(prob_dir = tempfile("no-such-probatlas-")),
    class = "neuroatlas_error_wang_prob"
  )
})

test_that("a partially cached file resolves from the cache (per-file)", {
  cache_root <- tempfile("wang-cache-")
  dir.create(cache_root, recursive = TRUE)
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit({
    if (is.na(old_cache)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache)
    }
  }, add = TRUE)

  # Seed the cache with ONLY maxprob (exists but lacks probability maps).
  cache_svd <- file.path(.neuroatlas_cache_dir("wang"), "subj_vol_all")
  dir.create(cache_svd, recursive = TRUE, showWarnings = FALSE)
  sp <- neuroim2::NeuroSpace(c(4L, 4L, 4L), spacing = c(2, 2, 2))
  for (h in c("lh", "rh")) {
    neuroim2::write_vol(
      neuroim2::NeuroVol(array(0L, c(4, 4, 4)), sp),
      file.path(cache_svd, paste0("maxprob_vol_", h, ".nii.gz"))
    )
  }

  # maxprob resolves from the cache; a manifest never downloads, so the
  # not-yet-cached probability maps remain unresolved.
  mp <- get_wang_prob_atlas(image = "maxprob", hemi = "both")
  expect_true(all(mp$files$exists))
  prob <- get_wang_prob_atlas(image = "probability", hemi = "both")
  expect_false(any(prob$files$exists))
})

test_that("a path_only manifest does not download or create the user cache", {
  cache_root <- tempfile("wang-cache-")
  dir.create(cache_root, recursive = TRUE)
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit({
    if (is.na(old_cache)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache)
    }
  }, add = TRUE)

  wang_cache <- file.path(tools::R_user_dir("neuroatlas", "cache"), "wang")
  expect_false(dir.exists(wang_cache))

  man <- get_wang_prob_atlas(image = "maxprob", hemi = "lh") # path_only = TRUE
  expect_s3_class(man, "wang_prob_paths")
  expect_false(any(man$files$exists)) # not downloaded by a manifest call
  # A manifest must not create or write to the user cache.
  expect_false(dir.exists(wang_cache))
})

test_that("get_wang_prob_atlas downloads and loads volumes on first use", {
  skip_on_cran()

  cache_root <- tempfile("wang-cache-")
  dir.create(cache_root, recursive = TRUE)
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit({
    if (is.na(old_cache)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache)
    }
  }, add = TRUE)

  mp <- tryCatch(
    get_wang_prob_atlas(image = "maxprob", hemi = "both", path_only = FALSE),
    error = function(e) skip(paste("Wang volume download unavailable:",
                                   conditionMessage(e)))
  )
  expect_s3_class(mp, "wang_prob_volumes")
  expect_named(mp$volumes, c("lh", "rh"))
  expect_s4_class(mp$volumes[["lh"]], "NeuroVol")
  # Max-prob volumes carry integer area labels 0..25.
  expect_true(max(mp$volumes[["lh"]]) <= 25)

  # Files are now cached: a subsequent manifest resolves offline.
  man <- get_wang_prob_atlas(image = "maxprob", hemi = "both")
  expect_true(all(man$files$exists))

  # A single probability map also downloads/resolves.
  v1v <- get_wang_prob_atlas(rois = "V1v", hemi = "lh", path_only = FALSE)
  expect_s4_class(v1v$volumes[[1]], "NeuroVol")
})

test_that("use_cache = FALSE loads from prob_dir without seeding the cache", {
  cache_root <- tempfile("wang-cache-")
  dir.create(cache_root, recursive = TRUE)
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit({
    if (is.na(old_cache)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache)
    }
  }, add = TRUE)

  root <- tempfile("ProbAtlas_v4-")
  svd <- file.path(root, "subj_vol_all")
  dir.create(svd, recursive = TRUE)
  sp <- neuroim2::NeuroSpace(c(4L, 4L, 4L), spacing = c(2, 2, 2))
  for (h in c("lh", "rh")) {
    vol <- neuroim2::NeuroVol(array(runif(64), c(4, 4, 4)), sp)
    neuroim2::write_vol(vol, file.path(svd, paste0("maxprob_vol_", h, ".nii.gz")))
  }

  loaded <- get_wang_prob_atlas(prob_dir = root, image = "maxprob",
                                hemi = "both", path_only = FALSE,
                                use_cache = FALSE)
  expect_s3_class(loaded, "wang_prob_volumes")

  wang_cache <- file.path(tools::R_user_dir("neuroatlas", "cache"), "wang")
  expect_false(dir.exists(wang_cache)) # nothing seeded
})

test_that("explicit prob_dir wins over a corrupt cache and repairs it", {
  cache_root <- tempfile("wang-cache-")
  dir.create(cache_root, recursive = TRUE)
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit({
    if (is.na(old_cache)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache)
    }
  }, add = TRUE)

  root <- tempfile("ProbAtlas_v4-")
  svd <- file.path(root, "subj_vol_all")
  dir.create(svd, recursive = TRUE)
  sp <- neuroim2::NeuroSpace(c(4L, 4L, 4L), spacing = c(2, 2, 2))
  src_file <- file.path(svd, "maxprob_vol_lh.nii.gz")
  neuroim2::write_vol(
    neuroim2::NeuroVol(array(runif(64), c(4, 4, 4)), sp), src_file
  )
  good_size <- file.size(src_file)

  # Poison the cache with a truncated file of the same basename.
  cache_svd <- file.path(.neuroatlas_cache_dir("wang"), "subj_vol_all")
  dir.create(cache_svd, recursive = TRUE, showWarnings = FALSE)
  bad <- file.path(cache_svd, "maxprob_vol_lh.nii.gz")
  writeBin(raw(3), bad)
  expect_false(isTRUE(file.size(bad) == good_size))

  loaded <- get_wang_prob_atlas(prob_dir = root, image = "maxprob",
                                hemi = "lh", path_only = FALSE)

  # Current call must read from prob_dir, not the (corrupt) cache.
  expect_equal(normalizePath(loaded$subj_vol_all), normalizePath(svd))
  expect_true(grepl(normalizePath(svd), normalizePath(loaded$files$path[1]),
                    fixed = TRUE))
  # And the corrupt cache entry must be repaired to match the source.
  expect_equal(file.size(bad), good_size)
})

test_that("get_wang_atlas loads an fsaverage surface atlas", {
  skip_on_cran()

  atlas <- wang_or_skip(get_wang_atlas()) # default surf = "midthickness"

  expect_s3_class(atlas, "wang")
  expect_s3_class(atlas, "surfatlas")
  expect_equal(atlas$surface_space, "fsaverage")
  expect_equal(atlas$surf_type, "midthickness")
  expect_s4_class(atlas$lh_atlas, "LabeledNeuroSurface")
  expect_s4_class(atlas$rh_atlas, "LabeledNeuroSurface")
  expect_equal(length(atlas$ids), 50L) # 25 areas x 2 hemispheres
  expect_true("hV4" %in% atlas$labels)
  expect_true(all(c("left", "right") %in% atlas$hemi))
  expect_equal(atlas_ref(atlas)$family, "wang")

  # Per-vertex codes follow the surfatlas convention shared with plot_brain():
  # 0 = unlabeled background, left areas = ids 1..25, right areas = ids 26..50.
  ld <- atlas$lh_atlas@data
  rd <- atlas$rh_atlas@data
  expect_true(any(ld == 0) && any(rd == 0))      # background present and culled
  expect_true(all(ld %in% c(0, 1:25)))
  expect_true(all(rd %in% c(0, 26:50)))
  expect_setequal(setdiff(unique(ld), 0), 1:25)  # all left areas represented
  expect_setequal(setdiff(unique(rd), 0), 26:50) # all right areas represented

  # "inflated" is intentionally not offered (TemplateFlow lacks it for fsaverage)
  expect_error(get_wang_atlas(surf = "inflated"))

  # get_roi() works on the surface atlas, returning ROISurface objects.
  roi <- get_roi(atlas, label = "hV4")
  expect_named(roi, c("hV4_left", "hV4_right"))
  expect_s4_class(roi[["hV4_left"]], "ROISurface")
  expect_true(length(neurosurf::indices(roi[["hV4_left"]])) > 0)

  # hemi filter narrows to one hemisphere.
  roi_l <- get_roi(atlas, label = "V1v", hemi = "left")
  expect_named(roi_l, "V1v_left")

  # Selecting by id (right-hemisphere id) resolves to the right mesh.
  rid <- atlas$ids[atlas$labels == "hV4" & atlas$hemi == "right"]
  roi_r <- get_roi(atlas, id = rid)
  expect_s4_class(roi_r[[1]], "ROISurface")
  # All ROI vertices carry that id's code on the right hemisphere.
  expect_true(all(as.integer(atlas$rh_atlas@data)[
    neurosurf::indices(roi_r[[1]])] == rid))
})
