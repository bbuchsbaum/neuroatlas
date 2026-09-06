make_toy_hcpex_assets <- function() {
  root <- tempfile("hcpex-source-")
  dir.create(root)
  sp <- neuroim2::NeuroSpace(c(32, 32, 32))
  arr <- array(0, c(32, 32, 32))
  arr[seq_len(427)] <- 0:426
  neuroim2::write_vol(neuroim2::NeuroVol(arr, sp),
                     file.path(root, "HCPex.nii.gz"))
  ids <- 1:426
  hemi <- c(rep("L", 180), rep("R", 180), rep("L", 33), rep("R", 33))
  short <- data.frame(ids, hemi, paste0("Region", ids), ids)
  full <- data.frame(0:426, c("Unknown", paste0("Full", ids, "_", hemi)),
                      red = c(0, rep(208, 426)), green = 0, blue = 117, alpha = 0)
  utils::write.table(short, file.path(root, "HCPex.nii.txt"),
                     row.names = FALSE, col.names = FALSE, quote = FALSE)
  utils::write.table(full, file.path(root, "HCPex_LookUpTable.txt"),
                     row.names = FALSE, col.names = FALSE, quote = FALSE)
  info <- .hcpex_source_info("1")
  paths <- file.path(root, info$file)
  info$size <- file.info(paths)$size
  info$md5 <- unname(tools::md5sum(paths))
  list(root = root, info = info, arr = arr, space = sp)
}

local_hcpex_downloads <- function(fixture, cache, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    .hcpex_source_info = function(resolution) fixture$info,
    .neuroatlas_cache_dir = function(...) {
      dir.create(cache, showWarnings = FALSE)
      cache
    },
    .neuroatlas_download = function(url, dest, ...) {
      file.copy(file.path(fixture$root, basename(url)), dest, overwrite = TRUE)
      dest
    },
    .env = .env
  )
}

test_that("HCPex loads offline through the registry with native IDs and metadata", {
  fixture <- make_toy_hcpex_assets()
  cache <- tempfile("hcpex-cache-")
  on.exit(unlink(c(fixture$root, cache), recursive = TRUE))
  local_hcpex_downloads(fixture, cache)
  a <- get_atlas("HCPex")
  expect_s3_class(a, "hcpex")
  expect_s3_class(a, "atlas")
  expect_equal(as.vector(a$atlas), as.vector(fixture$arr))
  expect_identical(a$ids, 1:426)
  expect_equal(a$labels[c(1, 181, 361, 394)],
               c("Region1_L", "Region181_R", "Region361_L", "Region394_R"))
  expect_equal(a$orig_labels[1], "Full1_L")
  expect_equal(as.integer(table(a$hemi)), c(213, 213))
  expect_equal(a$cmap$red[1], 208)
  expect_equal(a$cmap$blue[1], 117)
  m <- atlas_metadata(a)
  expect_equal(m$identity$version, "1.1")
  expect_equal(m$spatial$template_space, "MNI152NLin2009cAsym")
  expect_equal(m$spatial$basis, "source_declared")
  expect_equal(m$spatial$voxel_size, c(1, 1, 1))
  expect_length(atlas_citations(a), 3)
  expect_equal(m$artifacts$source_version, rep("1.1", 3))
  expect_equal(m$artifacts$license, rep("GPL-3.0", 3))
  expect_true(all(!is.na(m$artifacts$checksum)))
  expect_equal(m$artifacts$source_ref, fixture$info$revision)
  expect_identical(atlas_metadata(unserialize(serialize(a, NULL))), m)
  expect_equal(find_atlas_spec("hcp_extended")$id, "hcpex")

  # Cached files must suffice with downloads disabled.
  testthat::local_mocked_bindings(
    .neuroatlas_download = function(...) stop("unexpected download")
  )
  expect_equal(as.vector(get_hcpex_atlas()$atlas), as.vector(fixture$arr))
})

test_that("HCPex supports ROI extraction, filtering, mapping and reduction", {
  fixture <- make_toy_hcpex_assets()
  cache <- tempfile("hcpex-cache-")
  on.exit(unlink(c(fixture$root, cache), recursive = TRUE))
  local_hcpex_downloads(fixture, cache)
  a <- get_hcpex_atlas()
  roi <- get_roi(a, label = "Region361_L")
  expect_s4_class(roi[[1]], "ROIVol")
  expect_equal(as.numeric(roi[[1]]), 361)
  b <- filter_atlas(a, division == "subcortical", hemi == "left")
  expect_equal(b$ids, 361:393)
  expect_equal(roi_metadata(b)$region, paste0("Region", 361:393))
  expect_equal(atlas_metadata(b)$content$regions, 33)
  expect_equal(atlas_artifacts(b), atlas_artifacts(a))
  mapped <- map_atlas(a, seq_len(426) * 2)
  expected <- fixture$arr * 2
  expect_equal(mapped$statistic, seq_len(426) * 2)
  expect_equal(mapped$label, a$orig_labels)
  stats <- reduce_atlas(a, neuroim2::NeuroVol(expected, fixture$space), mean)
  expect_equal(stats$value, seq_len(426) * 2)
})

test_that("HCPex resamples labels while preserving source identity and receipts", {
  fixture <- make_toy_hcpex_assets()
  cache <- tempfile("hcpex-cache-")
  on.exit(unlink(c(fixture$root, cache), recursive = TRUE))
  local_hcpex_downloads(fixture, cache)
  out <- neuroim2::NeuroSpace(c(16, 16, 16), spacing = c(2, 2, 2))
  expect_warning(a <- get_hcpex_atlas(outspace = out), "labels were lost")
  expect_equal(dim(a$atlas), c(16, 16, 16))
  expected <- fixture$arr[seq(1, 31, 2), seq(1, 31, 2), seq(1, 31, 2)]
  expect_equal(as.vector(a$atlas), as.vector(expected))
  expect_true(all(as.array(a$atlas) %in% 0:426))
  expect_equal(a$ids, 1:426)
  m <- atlas_metadata(a)
  expect_equal(m$spatial$template_space, "MNI152NLin2009cAsym")
  expect_equal(m$spatial$voxel_size, c(2, 2, 2))
  expect_equal(m$artifacts$resolution[1], "1mm")
  expect_equal(m$history$action, c("load", "resample"))
  expect_equal(m$history$parameters[[2]]$interpolation, "nearest")
})

test_that("HCPex repairs corrupt caches and rejects corrupt downloads", {
  fixture <- make_toy_hcpex_assets()
  cache <- tempfile("hcpex-cache-")
  on.exit(unlink(c(fixture$root, cache), recursive = TRUE))
  local_hcpex_downloads(fixture, cache)
  info <- fixture$info[2, ]
  path <- .hcpex_asset_path(info)
  writeLines("corrupt", path)
  expect_equal(.hcpex_asset_path(info), path)
  expect_equal(unname(tools::md5sum(path)), info$md5)
  # Same-size corruption must fail too, and cannot replace a valid cache.
  testthat::local_mocked_bindings(.neuroatlas_download = function(url, dest, ...) {
    writeBin(as.raw(rep(0, info$size)), dest)
  })
  bad <- info
  bad$md5 <- paste(rep("0", 32), collapse = "")
  expect_error(.hcpex_asset_path(bad), "integrity check")
  expect_equal(unname(tools::md5sum(path)), info$md5)
  expect_equal(list.files(cache), info$file)
})

test_that("uncached HCPex downloads are removed after success and failure", {
  fixture <- make_toy_hcpex_assets()
  cache <- tempfile("hcpex-cache-")
  on.exit(unlink(c(fixture$root, cache), recursive = TRUE))
  local_hcpex_downloads(fixture, cache)
  a <- get_hcpex_atlas(use_cache = FALSE)
  expect_false(any(file.exists(atlas_artifacts(a)$local_path)))
  expect_false(dir.exists(cache))
  before <- list.files(tempdir(), pattern = "^hcpex-")
  testthat::local_mocked_bindings(.hcpex_read_labels = function(...) stop("bad labels"))
  expect_error(get_hcpex_atlas(use_cache = FALSE), "bad labels")
  expect_equal(list.files(tempdir(), pattern = "^hcpex-"), before)
})

test_that("HCPex rejects malformed labels and invalid arguments", {
  fixture <- make_toy_hcpex_assets()
  on.exit(unlink(fixture$root, recursive = TRUE))
  short <- file.path(fixture$root, "HCPex.nii.txt")
  full <- file.path(fixture$root, "HCPex_LookUpTable.txt")
  lines <- readLines(short)
  writeLines(rev(lines), short)
  expect_equal(.hcpex_read_labels(short, full)$id, 1:426)
  writeLines(c(lines[-1], lines[2]), short)
  expect_error(.hcpex_read_labels(short, full), "Invalid HCPex")
  writeLines(sub(" L ", " R ", lines), short)
  expect_error(.hcpex_read_labels(short, full), "Invalid HCPex")
  expect_error(get_hcpex_atlas(resolution = 3), "arg")
  expect_error(get_hcpex_atlas(use_cache = NA))
  expect_error(get_hcpex_atlas(outspace = "MNI152"))
  expect_equal(.hcpex_source_info("2")$file[1], "HCPex_2mm.nii")
})

test_that("upstream HCPex assets load at both native resolutions", {
  skip_on_cran()
  skip_if(Sys.getenv("NEUROATLAS_TEST_HCPEX_NETWORK") != "true",
          "Set NEUROATLAS_TEST_HCPEX_NETWORK=true for upstream integration")
  for (resolution in c(1, 2)) {
    a <- get_hcpex_atlas(resolution = resolution, use_cache = FALSE)
    expect_equal(sort(unique(as.vector(as.array(a$atlas)))), 0:426)
    expect_equal(neuroim2::spacing(a$atlas), rep(resolution, 3))
    expect_equal(dim(a$atlas), if (resolution == 1) c(193, 229, 193) else
                   c(91, 109, 91))
    expect_equal(a$labels[c(1, 181, 387, 420)],
                 c("V1_L", "V1_R", "Amyg_L", "Amyg_R"))
    expect_equal(a$orig_labels[1], "Primary_Visual_Cortex_L")
    expect_equal(unname(unlist(a$cmap[1, ])), c(208, 216, 117))
  }
})
