transform_cache_artifact <- function(bytes = charToRaw("verified transform")) {
  path <- tempfile("transform-fixture-")
  writeBin(bytes, path)
  data.frame(
    artifact_id = "mni6_to_mni2009",
    artifact_version = "v1.0.0",
    provider = "release",
    url = "https://example.org/releases/v1.0.0/mni6_to_mni2009.h5",
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE),
    size_bytes = length(bytes),
    format = "ants_h5",
    qualification = "passed",
    status = "available",
    stringsAsFactors = FALSE
  )
}


test_that("valid cached transform works offline", {
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  artifact <- transform_cache_artifact()
  path <- file.path(cache, artifact$artifact_version, paste0(artifact$artifact_id, ".h5"))
  dir.create(dirname(path), recursive = TRUE)
  writeBin(charToRaw("verified transform"), path)
  local_mocked_bindings(.neuroatlas_download = function(...) stop("network used"),
                        .package = "neuroatlas")

  expect_identical(neuroatlas:::.fetch_transform_artifact(artifact, cache, offline = TRUE),
                   normalizePath(path))
})


test_that("missing offline transform never downloads", {
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  calls <- 0L
  local_mocked_bindings(.neuroatlas_download = function(...) {
    calls <<- calls + 1L
  }, .package = "neuroatlas")

  expect_error(neuroatlas:::.fetch_transform_artifact(
    transform_cache_artifact(), cache, offline = TRUE
  ), "download is disabled")
  expect_identical(calls, 0L)
})


test_that("downloaded transforms are verified before atomic publication", {
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  artifact <- transform_cache_artifact()
  local_mocked_bindings(.neuroatlas_download = function(url, dest, ...) {
    writeBin(charToRaw("verified transform"), dest)
    dest
  }, .package = "neuroatlas")

  path <- neuroatlas:::.fetch_transform_artifact(artifact, cache)
  expect_true(file.exists(path))
  expect_identical(readBin(path, "raw", n = 100L), charToRaw("verified transform"))
})


test_that("checksum failures do not poison the cache and remove partials", {
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  artifact <- transform_cache_artifact()
  artifact$sha256 <- paste0("0", substr(artifact$sha256, 2L, 64L))
  local_mocked_bindings(.neuroatlas_download = function(url, dest, ...) {
    writeBin(charToRaw("verified transform"), dest)
    dest
  }, .package = "neuroatlas")

  expect_error(neuroatlas:::.fetch_transform_artifact(artifact, cache), "integrity")
  version_dir <- file.path(cache, artifact$artifact_version)
  expect_false(file.exists(file.path(version_dir, paste0(artifact$artifact_id, ".h5"))))
  expect_length(list.files(version_dir, pattern = "\\.part$"), 0L)
})


test_that("size failures do not publish a transform artifact", {
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  artifact <- transform_cache_artifact()
  artifact$size_bytes <- artifact$size_bytes + 1L
  local_mocked_bindings(.neuroatlas_download = function(url, dest, ...) {
    writeBin(charToRaw("verified transform"), dest)
    dest
  }, .package = "neuroatlas")

  expect_error(neuroatlas:::.fetch_transform_artifact(artifact, cache), "integrity")
  expect_false(file.exists(file.path(
    cache, artifact$artifact_version, paste0(artifact$artifact_id, ".h5")
  )))
})


test_that("unsafe artifact paths and clear requests are refused", {
  artifact <- transform_cache_artifact()
  artifact$artifact_id <- "../escape"
  expect_error(neuroatlas:::.fetch_transform_artifact(artifact, tempfile()), "safe single")
  expect_error(neuroatlas::clear_transform_cache("../escape", tempfile()), "safe single")
  expect_error(neuroatlas::clear_transform_cache(cache_dir = "/"), "Unsafe")
})


test_that("filesystem roots and TemplateFlow paths are rejected before access", {
  local_mocked_bindings(.transform_cache_is_symlink = function(...) {
    stop("filesystem accessed")
  }, .package = "neuroatlas")
  paths <- c("/", "//", "C:/", "C:\\", "C:",
    "//server", "//server/share/", "\\\\server\\share",
    "\\\\?\\C:\\", "C:\\work\\TemplateFlow\\transforms",
    file.path(tempdir(), "templateflow", "transforms"))
  for (path in paths) {
    expect_error(clear_transform_cache(cache_dir = path), "Unsafe")
  }
})


test_that("clear removes only owned artifacts and refuses active locks", {
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  version_dir <- file.path(cache, "v1.0.0")
  dir.create(version_dir, recursive = TRUE)
  writeBin(charToRaw("x"), file.path(version_dir, "owned.h5"))
  write.dcf(data.frame(owner = "neuroatlas-transform-cache-v1", file = "owned.h5"),
            file.path(version_dir, "owned.h5.neuroatlas-receipt"))
  writeBin(charToRaw("unrelated"), file.path(version_dir, "unrelated.h5"))
  writeLines("keep", file.path(version_dir, "notes.txt"))
  expect_identical(neuroatlas::clear_transform_cache(cache_dir = cache), 1L)
  expect_true(file.exists(file.path(version_dir, "notes.txt")))
  expect_true(file.exists(file.path(version_dir, "unrelated.h5")))

  dir.create(file.path(version_dir, ".owned.lock"))
  expect_error(neuroatlas::clear_transform_cache(cache_dir = cache), "lock")
})

test_that("clear never treats an arbitrary H5 directory as owned", {
  root <- tempfile("unrelated-h5-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "experiment"), recursive = TRUE)
  path <- file.path(root, "experiment", "subject.h5")
  writeBin(charToRaw("keep"), path)
  expect_identical(clear_transform_cache(cache_dir = root), 0L)
  expect_true(file.exists(path))
})


test_that("active artifact locks time out without removal", {
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  artifact <- transform_cache_artifact()
  version_dir <- file.path(cache, artifact$artifact_version)
  dir.create(file.path(version_dir, paste0(".", artifact$artifact_id, ".lock")),
             recursive = TRUE)
  old <- options(neuroatlas.transform_cache_lock_timeout = 0)
  on.exit(options(old), add = TRUE)
  expect_error(neuroatlas:::.fetch_transform_artifact(artifact, cache), "Timed out")
  expect_true(dir.exists(file.path(version_dir, paste0(".", artifact$artifact_id, ".lock"))))
})

test_that("clearing and fetching share exclusion through the cache lock", {
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  dir.create(cache)
  artifact <- transform_cache_artifact()
  old <- options(neuroatlas.transform_cache_lock_timeout = 0)
  on.exit(options(old), add = TRUE)
  root_lock <- file.path(cache, ".neuroatlas-cache.lock")
  neuroatlas:::.with_transform_cache_lock(root_lock, NULL, NULL, function() {
    expect_error(clear_transform_cache(cache_dir = cache), "Timed out")
    expect_error(neuroatlas:::.fetch_transform_artifact(artifact, cache), "Timed out")
    expect_true(dir.exists(root_lock))
  })
  expect_false(dir.exists(root_lock))
})

test_that("upstream runtime verification cannot qualify a newly fitted artifact", {
  artifact <- transform_cache_artifact()
  artifact$qualification <- "runtime_verified"
  artifact$qualification_scope <- "upstream_transform_application"
  artifact$provider <- "templateflow"
  expect_silent(neuroatlas:::.validate_transform_artifact(artifact))
  artifact$provider <- "neuroatlas"
  expect_error(neuroatlas:::.validate_transform_artifact(artifact), "qualified")
  artifact$provider <- "templateflow"
  artifact$qualification_scope <- NA_character_
  expect_error(neuroatlas:::.validate_transform_artifact(artifact), "qualified")
})
