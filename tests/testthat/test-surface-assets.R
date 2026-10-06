test_that(
  "surface inputs share verified cache and selective cleanup",
  {
    bytes <- charToRaw("pinned surface bytes")
    entry <- list(
      sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE),
      size_bytes = length(bytes),
      url = "https://example.org/pinned/sphere.gii"
    )
    cache <- tempfile("surface-input-cache-")
    on.exit(unlink(cache, recursive = TRUE), add = TRUE)
    artifact <- neuroatlas:::.locked_surface_artifact(entry, "surface_gifti")
    local_mocked_bindings(
      .neuroatlas_download = function(url, dest, ...) {
        writeBin(bytes, dest)
      },
      .package = "neuroatlas"
    )
    path <- neuroatlas:::.fetch_transform_artifact(artifact, cache)
    expect_match(path, "[.]gii$")
    expect_identical(readBin(path, "raw", n = length(bytes)), bytes)
    expect_identical(
      neuroatlas:::.fetch_transform_artifact(
        artifact,
        cache,
        offline = TRUE
      ),
      path
    )
    writeBin(charToRaw("corrupt"), path)
    expect_error(
      neuroatlas:::.fetch_transform_artifact(
        artifact,
        cache,
        offline = TRUE
      ),
      "integrity"
    )
    expect_equal(clear_transform_cache(cache_dir = cache), 1L)
    expect_error(
      neuroatlas:::.fetch_transform_artifact(
        artifact,
        cache,
        offline = TRUE
      ),
      "disabled"
    )
    artifact$qualification_scope <- "unreviewed"
    expect_error(
      neuroatlas:::.fetch_transform_artifact(artifact, cache),
      "checksum-locked"
    )
  }
)

test_that(
  "only an exact regular tar member is read, without extraction",
  {
    skip_if(!nzchar(Sys.which("tar")), "tar utility unavailable")
    root <- tempfile("surface-tar-fixture-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    withr::local_dir(root)
    dir.create("atlas")
    bytes <- charToRaw("exact mask bytes")
    writeBin(bytes, "atlas/mask.gii")
    utils::tar(
      "fixture.tar.gz",
      files = "atlas/mask.gii",
      compression = "gzip",
      tar = Sys.which("tar")
    )
    unlink("atlas", recursive = TRUE)
    expect_identical(
      neuroatlas:::.surface_tar_member(
        "fixture.tar.gz",
        "atlas/mask.gii"
      ),
      bytes
    )
    expect_false(dir.exists("atlas"))
    expect_error(
      neuroatlas:::.surface_tar_member("fixture.tar.gz", "missing.gii"),
      "unavailable"
    )
  }
)

test_that(
  "unpinned surface families and densities fail before downloading",
  {
    skip_if_not_installed("gifti")
    skip_if_not_installed("jsonlite")
    local_mocked_bindings(
      .neuroatlas_download = function(...) stop("network used"),
      .package = "neuroatlas"
    )
    expect_error(get_surface_geometry("fsaverage", "10k", "L"), "No pinned")
    expect_error(get_surface_geometry("fsLR", "32k", "both"), "No pinned")
  }
)
