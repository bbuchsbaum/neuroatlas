#' Load the Extended HCP Atlas (HCPex)
#'
#' Load HCPex v1.1, a volumetric atlas with 360 cortical and 66 subcortical
#' regions in the source-declared MNI152NLin2009cAsym template space.
#'
#' @param resolution Native voxel size in millimetres: `1` (default) or `2`.
#' @param outspace Optional three-dimensional `NeuroSpace`. Resampling uses
#'   nearest neighbours and changes the sampling grid only; it does not register
#'   the atlas to a different anatomical template. Small regions may disappear.
#' @param use_cache Logical. Reuse checksum-verified files in the neuroatlas
#'   user cache. If `FALSE`, download temporary files and remove them after loading.
#'
#' @details
#' Files are downloaded on demand from a pinned revision of the upstream v1.1
#' distribution. Both native resolutions retain the upstream voxel geometry and
#' HCPex IDs (1:426, with zero as background). HCPex cortical ordering differs
#' from the original HCP-MMP1.0 ordering; matrices in that ordering must be
#' reordered before use with this atlas.
#'
#' Labels combine upstream abbreviations with hemisphere suffixes (e.g.,
#' `V1_L`, `V1_R`). `orig_labels` preserves the full lookup-table names.
#' [roi_metadata()] also includes `region` (the unsuffixed abbreviation) and
#' `division` (`"cortical"` for IDs 1:360, `"subcortical"` for 361:426).
#' The complete region catalogue is retained after resampling, even when some
#' regions no longer have voxels. No surface representation is supplied.
#'
#' The upstream data are distributed under GPL-3.0; their license, source
#' revision, file receipts, and publication references are attached to the atlas.
#'
#' @return An object with classes `hcpex` and `atlas`, containing a `NeuroVol`,
#'   region IDs, labels, hemisphere assignments, RGB colors, and resource metadata.
#' @source <https://github.com/wayalan/HCPex>, including the v1.1 user guide.
#' @references
#' Huang CC, Rolls ET, Feng J, Lin CP (2022). An extended Human Connectome
#' Project multimodal parcellation atlas of the human cortex and subcortical
#' areas. Brain Structure and Function, 227, 763-778.
#' \doi{10.1007/s00429-021-02421-6}
#'
#' Huang CC, Rolls ET, Hsu CH, Feng J, Lin CP (2021). Extensive Cortical
#' Connectivity of the Human Hippocampal Memory System: Beyond the "What" and
#' "Where" Dual Stream Model. Cerebral Cortex. \doi{10.1093/cercor/bhab113}
#' @seealso [get_atlas()], [get_glasser_atlas()], [atlas_citations()]
#' @examples
#' \dontrun{
#' hcp <- get_hcpex_atlas(resolution = 2)
#' # Also available through get_atlas("hcpex", resolution = 2)
#' get_roi(hcp, label = "V1_L")
#' subcortex <- filter_atlas(hcp, division == "subcortical")
#' atlas_metadata(hcp)
#' atlas_citations(hcp)
#' }
#' @md
#' @export
get_hcpex_atlas <- function(resolution = c("1", "2"), outspace = NULL,
                            use_cache = TRUE) {
  resolution <- match.arg(as.character(resolution), c("1", "2"))
  assertthat::assert_that(assertthat::is.flag(use_cache), !is.na(use_cache))
  if (!is.null(outspace)) {
    assertthat::assert_that(methods::is(outspace, "NeuroSpace"),
                           length(dim(outspace)) == 3L)
  }
  info <- .hcpex_source_info(resolution)
  paths <- character()
  on.exit(if (!use_cache) unlink(paths), add = TRUE)
  for (i in seq_len(nrow(info))) {
    paths <- c(paths, .hcpex_asset_path(info[i, ], use_cache))
  }
  labels <- .hcpex_read_labels(paths[[2]], paths[[3]])
  vol <- neuroim2::read_vol(paths[[1]])
  ids <- sort(unique(as.vector(as.array(vol))))
  if (length(dim(vol)) != 3L || !identical(as.numeric(ids), as.numeric(0:426))) {
    cli::cli_abort("HCPex volume must contain background and region IDs 1:426.",
                   class = c("neuroatlas_error_invalid_atlas", "neuroatlas_error"))
  }
  native_space <- "MNI152NLin2009cAsym"
  template_space <- .template_space_from_outspace(outspace, native_space)
  if (!is.null(outspace)) vol <- resample(vol, outspace, interp = 0)
  ref <- new_atlas_ref(
    family = "hcpex", model = "HCPex", representation = "volume",
    template_space = template_space, coord_space = "MNI152",
    resolution = paste0(resolution, "mm"),
    provenance = "https://github.com/wayalan/HCPex", source = "HCPex_v1.1",
    lineage = "Upstream HCPex v1.1 volume and label tables; native HCPex ordering.",
    confidence = if (is.null(outspace)) "high" else "approximate",
    notes = "Template identity is declared in the HCPex v1.1 user guide."
  )
  artifacts <- dplyr::bind_rows(lapply(seq_len(nrow(info)), function(i) {
    .new_atlas_artifact(
      role = info$role[[i]], family = "hcpex", model = "HCPex",
      source_name = "HCPex", source_url = info$url[[i]],
      source_ref = info$revision[[i]], source_version = "1.1",
      citation_doi = "10.1007/s00429-021-02421-6",
      license = "GPL-3.0",
      license_url = paste0("https://github.com/wayalan/HCPex/blob/",
                           info$revision[[i]], "/LICENSE"),
      file_name = info$file[[i]], local_path = paths[[i]],
      template_space = native_space, coord_space = "MNI152",
      resolution = if (i == 1L) paste0(resolution, "mm") else NA_character_,
      confidence = "high"
    )
  }))
  history <- .new_atlas_history(
    action = "load", representation = "volume",
    from_template_space = native_space, to_template_space = native_space,
    from_coord_space = "MNI152", to_coord_space = "MNI152",
    status = "available", confidence = "high",
    details = "Loaded checksum-verified HCPex v1.1 assets."
  )
  new_atlas(
    name = "HCPex-426", atlas = vol, ids = labels$id,
    labels = labels$label, orig_labels = labels$label_full,
    hemi = labels$hemi, cmap = labels[c("red", "green", "blue")],
    subclass = "hcpex",
    extra = list(region = labels$region, division = labels$division),
    ref = ref, artifacts = artifacts, history = history,
    metadata = list(
      parameters = list(version = "1.1", resolution = resolution),
      processing = attr(vol, "neuroatlas_processing", exact = TRUE)
    )
  )
}

#' @keywords internal
#' @noRd
.hcpex_source_info <- function(resolution) {
  revision <- "6d4082fcbfdb6814fc21dff82ff90b6c4ae33f30"
  info <- tibble::tibble(
    role = c("parcellation_volume", "label_table", "color_table"),
    file = c(if (resolution == "1") "HCPex.nii.gz" else "HCPex_2mm.nii",
             "HCPex.nii.txt", "HCPex_LookUpTable.txt"),
    md5 = c(if (resolution == "1") "45466c3747fca4ac8966393b21488cee" else
              "1cd112b5c9d810a37e23b156564fc19f",
            "e5ef202ae052d4a94ee58a881616df75",
            "022f90ce2ad495f1ed7bd6ff2461ef43"),
    size = c(if (resolution == "1") 751874 else 7221384, 5851, 15838),
    revision = revision
  )
  info$url <- paste0("https://raw.githubusercontent.com/wayalan/HCPex/",
                      revision, "/HCPex_v1.1/", info$file)
  info
}

#' @keywords internal
#' @noRd
.hcpex_asset_path <- function(info, use_cache = TRUE) {
  cache_dir <- if (use_cache) {
    .neuroatlas_cache_dir(file.path("hcpex", "1.1", info$revision))
  } else tempdir()
  path <- file.path(cache_dir, info$file)
  valid <- function(p) {
    file.exists(p) && !dir.exists(p) && file.info(p)$size == info$size &&
      identical(unname(tools::md5sum(p)), info$md5)
  }
  if (use_cache && valid(path)) return(path)
  tmp <- tempfile("hcpex-", tmpdir = cache_dir,
                  fileext = if (grepl("nii.gz$", info$file)) ".nii.gz" else
                    paste0(".", tools::file_ext(info$file)))
  keep <- FALSE
  on.exit(if (!keep) unlink(tmp), add = TRUE)
  .neuroatlas_download(info$url, dest = tmp, min_size = info$size,
                       description = paste("HCPex", info$file))
  if (!valid(tmp)) {
    cli::cli_abort("HCPex file failed its size or MD5 integrity check: {info$file}.",
                   class = c("neuroatlas_error_download", "neuroatlas_error"))
  }
  if (!use_cache) {
    keep <- TRUE
    return(tmp)
  }
  if (file.exists(path) && unlink(path) != 0L) {
    cli::cli_abort("Failed to replace invalid HCPex cache file at {.path {path}}.",
                   class = c("neuroatlas_error_cache", "neuroatlas_error"))
  }
  if (!file.rename(tmp, path)) {
    cli::cli_abort("Failed to publish HCPex cache file at {.path {path}}.",
                   class = c("neuroatlas_error_cache", "neuroatlas_error"))
  }
  path
}

#' @keywords internal
#' @noRd
.hcpex_read_labels <- function(label_path, color_path) {
  short <- utils::read.table(label_path, header = FALSE, comment.char = "",
                             colClasses = c("integer", "character",
                                            "character", "integer"))
  full <- utils::read.table(color_path, header = FALSE, comment.char = "#",
                            colClasses = c("integer", "character",
                                           rep("integer", 4)))
  fail <- function() cli::cli_abort(
    "Invalid HCPex label tables: expected aligned IDs, hemispheres, and RGB colors.",
    class = c("neuroatlas_error_invalid_atlas", "neuroatlas_error"))
  if (ncol(short) != 4L || ncol(full) != 6L) fail()
  full <- full[full[[1]] != 0L, , drop = FALSE]
  short <- short[order(short[[1]]), , drop = FALSE]
  full <- full[order(full[[1]]), , drop = FALSE]
  if (!identical(short[[1]], 1:426) || !identical(full[[1]], 1:426) ||
      !identical(short[[4]], 1:426) || anyNA(short) || anyNA(full)) fail()
  hemi <- ifelse(short[[1]] <= 180L |
                   (short[[1]] >= 361L & short[[1]] <= 393L), "L", "R")
  rgb <- unname(as.matrix(full[3:5]))
  if (!identical(short[[2]], hemi) ||
      !all(endsWith(full[[2]], paste0("_", hemi))) ||
      any(rgb < 0L | rgb > 255L)) fail()
  labels <- paste(short[[3]], hemi, sep = "_")
  if (anyDuplicated(labels) || anyDuplicated(full[[2]])) fail()
  tibble::tibble(
    id = short[[1]], label = labels, label_full = full[[2]],
    hemi = ifelse(hemi == "L", "left", "right"), region = short[[3]],
    division = ifelse(short[[1]] <= 360L, "cortical", "subcortical"),
    red = rgb[, 1], green = rgb[, 2], blue = rgb[, 3]
  )
}
