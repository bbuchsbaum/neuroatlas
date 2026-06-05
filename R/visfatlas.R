#' visfAtlas Probabilistic Functional Visual Atlas (volume)
#'
#' @description
#' Load the Rosenke et al. (2021) probabilistic functional atlas of human
#' occipito-temporal visual cortex ("visfAtlas") as a volumetric MNI atlas.
#' The maximum-probability labelmap contains 33 regions spanning the early
#' retinotopic areas (\code{v1d}, \code{v2d}, \code{v3d}, \code{v1v},
#' \code{v2v}, \code{v3v}), motion-selective \code{hMT}, and category-selective
#' regions for faces, bodies, characters, and places.
#'
#' @details
#' The atlas is distributed as a single archive
#' (\code{visfAtlas.zip}, ~70 MB) containing FreeSurfer, BrainVoyager, and
#' NIfTI representations. This loader downloads the archive on demand, extracts
#' the volumetric maximum-probability map
#' (\code{visfAtlas_MNI152_volume.nii.gz}, 1 mm), and caches both under the
#' neuroatlas user cache directory. Region intensities (1-33) follow the
#' distributed FSL atlas specification; region names use the lower/upper-case
#' source labels with hemisphere prefixes \code{lh_}/\code{rh_}.
#'
#' The volume is a single-subject MNI-space grid (182 x 218 x 182, 1 mm); the
#' publication aligns it to the MNI colin27 brain.
#'
#' Note that the visfAtlas defines V1-V3 (dorsal and ventral) but not hV4; for
#' V4 see \code{\link{get_wang_atlas}} or \code{\link{get_visual_atlas}}.
#'
#' @param outspace Optional \code{NeuroSpace} object (or TemplateFlow-style
#'   descriptor) to resample the atlas into.
#' @param smooth Logical. Whether to smooth parcel boundaries when resampling.
#' @param use_cache Logical. Whether to use cached downloads. Default
#'   \code{TRUE}.
#'
#' @return A list with classes \code{c("visfatlas", "volatlas", "atlas")}.
#'
#' @references
#' Rosenke, M., van Hoof, R., van den Hurk, J., Grill-Spector, K., & Goebel, R.
#' (2021). A Probabilistic Functional Atlas of Human Occipito-Temporal Visual
#' Cortex. Cerebral Cortex, 31(1), 603-619. \doi{10.1093/cercor/bhaa246}
#'
#' @source \url{https://download.brainvoyager.com/data/visfAtlas.zip}
#'
#' @seealso \code{\link{get_wang_atlas}}, \code{\link{get_visual_atlas}}.
#'
#' @examples
#' \dontrun{
#' visf <- get_visfatlas()
#' table(visf$hemi)
#' get_roi(visf, label = "lh_v1d_retinotopic")
#' }
#'
#' @importFrom neuroim2 read_vol ClusteredNeuroVol
#' @importFrom utils unzip
#' @importFrom assertthat assert_that
#' @export
get_visfatlas <- function(outspace = NULL,
                          smooth = FALSE,
                          use_cache = TRUE) {
  template_space <- .template_space_from_outspace(
    outspace,
    default_space = "MNI152"
  )

  if (!is.null(outspace) && !methods::is(outspace, "NeuroSpace")) {
    outspace <- .resolve_template_input(outspace, target_type = "NeuroSpace")
    if (is.null(outspace) || !methods::is(outspace, "NeuroSpace")) {
      stop("'outspace' must resolve to a valid NeuroSpace object")
    }
  }

  vol_path <- .visfatlas_volume_path(use_cache = use_cache)
  labels <- .visfatlas_labels()
  vol <- neuroim2::read_vol(vol_path)

  if (!is.null(outspace)) {
    assertthat::assert_that(length(dim(outspace)) == 3)
    vol <- resample(vol, outspace, smooth)
  }

  actual_ids <- sort(unique(as.integer(vol[vol != 0])))
  if (length(actual_ids) == 0L) {
    stop("No visfAtlas regions remain after resampling")
  }

  labels <- labels[match(actual_ids, labels$id), , drop = FALSE]
  if (anyNA(labels$id)) {
    stop("visfAtlas label table is missing ids present in the volume")
  }

  label_map <- as.list(actual_ids)
  names(label_map) <- labels$label
  vol <- neuroim2::ClusteredNeuroVol(
    as.logical(vol),
    clusters = vol[vol != 0],
    label_map = label_map
  )

  ref <- new_atlas_ref(
    family = "visfatlas",
    model = "visfAtlas2021",
    representation = "volume",
    template_space = template_space,
    coord_space = "MNI152",
    resolution = "1mm",
    provenance = "https://download.brainvoyager.com/data/visfAtlas.zip",
    source = "brainvoyager_download",
    lineage = "Rosenke et al. 2021 maximum-probability volume (NVA to MNI).",
    confidence = if (is.null(outspace)) "high" else "approximate",
    notes = paste(
      "Single-subject MNI-space grid (182x218x182, 1mm);",
      "publication aligns the volume to MNI colin27.",
      "Downloaded on demand; not bundled with neuroatlas."
    )
  )

  artifacts <- dplyr::bind_rows(
    .new_atlas_artifact(
      role = "summary_label_volume",
      family = "visfatlas",
      model = "visfAtlas2021",
      source_name = "BrainVoyager",
      source_url = "https://download.brainvoyager.com/data/visfAtlas.zip",
      source_ref = "visfAtlas_MNI152_volume.nii.gz",
      citation_doi = "10.1093/cercor/bhaa246",
      file_name = "visfAtlas_MNI152_volume.nii.gz",
      template_space = "MNI152",
      coord_space = "MNI152",
      resolution = "1mm",
      parcels = "33",
      lineage = "visfAtlas maximum-probability MNI volume.",
      confidence = "high",
      notes = "Downloaded on demand; not bundled with neuroatlas."
    ),
    .new_atlas_artifact(
      role = "label_table",
      family = "visfatlas",
      model = "visfAtlas2021",
      source_name = "BrainVoyager",
      source_url = "https://download.brainvoyager.com/data/visfAtlas.zip",
      source_ref = "visfAtlas_FSL.xml",
      citation_doi = "10.1093/cercor/bhaa246",
      file_name = "visfAtlas_FSL.xml",
      lineage = "Distributed FSL atlas specification (intensity-to-ROI mapping).",
      confidence = "high"
    )
  )

  history <- .new_atlas_history(
    action = "load",
    representation = "volume",
    from_template_space = "MNI152",
    to_template_space = template_space,
    from_coord_space = "MNI152",
    to_coord_space = "MNI152",
    status = "available",
    confidence = if (is.null(outspace)) "high" else "approximate",
    details = "Loaded visfAtlas maximum-probability MNI volume."
  )

  new_atlas(
    name = "visfAtlas",
    atlas = vol,
    ids = actual_ids,
    labels = labels$label,
    orig_labels = labels$orig_label,
    hemi = labels$hemi,
    cmap = labels[, c("red", "green", "blue")],
    subclass = c("visfatlas", "volatlas"),
    extra = list(
      region = labels$region,
      category = labels$category
    ),
    ref = ref,
    artifacts = artifacts,
    history = history
  )
}


#' Download + extract + cache the visfAtlas MNI volume.
#'
#' @keywords internal
#' @noRd
.visfatlas_volume_path <- function(use_cache = TRUE) {
  cache_dir <- .neuroatlas_cache_dir("visfatlas")
  vol_name <- "visfAtlas_MNI152_volume.nii.gz"
  vol_path <- file.path(cache_dir, vol_name)

  if (use_cache && file.exists(vol_path)) {
    return(vol_path)
  }

  zip_path <- file.path(cache_dir, "visfAtlas.zip")
  if (!use_cache || !file.exists(zip_path)) {
    .neuroatlas_download(
      url = "https://download.brainvoyager.com/data/visfAtlas.zip",
      dest = zip_path,
      min_size = 1000000L,
      description = "visfAtlas archive (visfAtlas.zip)"
    )
  }

  member <- "visfAtlas/nifti_volume/visfAtlas_MNI152_volume.nii.gz"
  tmp <- tempfile("visfatlas")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  extracted <- utils::unzip(zip_path, files = member, exdir = tmp,
                            junkpaths = TRUE)
  if (length(extracted) == 0L || !file.exists(extracted[1])) {
    cli::cli_abort(
      c(
        "visfAtlas archive did not contain the expected volume file.",
        "i" = "Expected member: {.path {member}}."
      ),
      class = c("neuroatlas_error_visfatlas", "neuroatlas_error")
    )
  }

  ok <- file.copy(extracted[1], vol_path, overwrite = TRUE)
  if (!ok) {
    cli::cli_abort(
      "Failed to cache visfAtlas volume at {.path {vol_path}}.",
      class = c("neuroatlas_error_cache", "neuroatlas_error")
    )
  }
  vol_path
}


#' visfAtlas region label table (intensity value -> ROI).
#'
#' Faithful to the distributed \code{visfAtlas_FSL.xml} (intensity = XML index
#' + 1). Source label typos (\code{lh_IOS_haracters}, trailing spaces) are kept
#' in \code{orig_label} but corrected in \code{label}.
#'
#' @keywords internal
#' @noRd
.visfatlas_labels <- function() {
  orig <- c(
    "lh_mFus_faces", "lh_pFus_faces", "lh_IOG_faces", "lh_OTS_bodies",
    "lh_ITG_bodies", "lh_MTG_bodies", "lh_LOS_bodies", "lh_pOTS_characters",
    "lh_IOS_haracters", "lh_CoS_places", "lh_hMT_motion",
    "lh_v1d_retinotopic", "lh_v2d_retinotopic", "lh_v3d_retinotopic",
    "lh_v1v_retinotopic", "lh_v2v_retinotopic", "lh_v3v_retinotopic",
    "rh_mFus_faces", "rh_pFus_faces", "rh_IOG_faces", "rh_OTS_bodies",
    "rh_ITG_bodies", "rh_MTG_bodies", "rh_LOS_bodies", "rh_CoS_places",
    "rh_TOS_places", "rh_hMT_motion",
    "rh_v1d_retinotopic", "rh_v2d_retinotopic", "rh_v3d_retinotopic",
    "rh_v1v_retinotopic", "rh_v2v_retinotopic", "rh_v3v_retinotopic"
  )
  label <- sub("_haracters", "_characters", trimws(orig))

  parts <- strsplit(label, "_", fixed = TRUE)
  hemi_pref <- vapply(parts, `[[`, character(1), 1L)
  hemi <- ifelse(hemi_pref == "lh", "left",
                 ifelse(hemi_pref == "rh", "right", NA_character_))
  region <- vapply(parts, `[[`, character(1), 2L)
  category <- vapply(parts, function(p) p[[length(p)]], character(1))

  rgb <- t(grDevices::col2rgb(grDevices::rainbow(length(label))))
  colnames(rgb) <- c("red", "green", "blue")

  tibble::tibble(
    id = seq_along(label),
    label = label,
    orig_label = orig,
    hemi = hemi,
    region = region,
    category = category,
    red = rgb[, "red"],
    green = rgb[, "green"],
    blue = rgb[, "blue"]
  )
}
