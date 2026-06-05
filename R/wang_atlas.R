#' Wang (2015) Probabilistic Visual Topography Atlas
#'
#' @description
#' Load the Wang et al. (2015) probabilistic atlas of visual topographic areas
#' as a pair of neurosurf \code{LabeledNeuroSurface} objects on the FreeSurfer
#' \code{fsaverage} surface. The atlas defines 25 topographic areas per
#' hemisphere, covering the early visual areas (\code{V1v}, \code{V1d},
#' \code{V2v}, \code{V2d}, \code{V3v}, \code{V3d}, \code{hV4}) plus ventral,
#' lateral, dorsal, and parietal maps (\code{VO1/2}, \code{PHC1/2},
#' \code{LO1/2}, \code{TO1/2}, \code{V3A/B}, \code{IPS0-5}, \code{SPL1},
#' \code{FEF}).
#'
#' @details
#' The surface labels are the maximum-probability map (MPM) derived from 53
#' subjects, distributed as \code{fsaverage} (164k vertices) FreeSurfer overlay
#' files (\code{lh/rh.wang15_mplbl.v1_0.mgz}) bundled with the \pkg{neuropythy}
#' library. Files are downloaded on demand from the neuropythy GitHub
#' repository and cached under the neuroatlas user cache directory.
#'
#' Surface geometry is obtained from TemplateFlow via
#' \code{\link{get_surface_template}}, so a working TemplateFlow setup is
#' required (mirroring \code{\link{glasser_surf}}). The full per-area
#' probability maps are also available from the original Princeton
#' distribution and may be added in a future release.
#'
#' @param surf Surface type. One of \code{"inflated"} (default),
#'   \code{"pial"}, \code{"white"}, or \code{"midthickness"}.
#' @param space Surface space / mesh template. Only \code{"fsaverage"}
#'   (164k vertices) is supported, matching the native atlas resolution.
#' @param use_cache Logical. Whether to use cached downloads. Default
#'   \code{TRUE}.
#'
#' @return A list with classes \code{c("wang", "surfatlas", "atlas")}
#'   containing \code{lh_atlas}/\code{rh_atlas} (\code{LabeledNeuroSurface}
#'   objects), \code{ids}, \code{labels}, \code{hemi}, \code{cmap}, and the
#'   standard atlas provenance metadata.
#'
#' @references
#' Wang, L., Mruczek, R. E. B., Arcaro, M. J., & Kastner, S. (2015).
#' Probabilistic Maps of Visual Topography in Human Cortex. Cerebral Cortex,
#' 25(10), 3911-3931. \doi{10.1093/cercor/bhu277}
#'
#' @source
#' \url{https://github.com/noahbenson/neuropythy} (bundled Wang 2015 atlas);
#' original distribution at \url{https://scholar.princeton.edu/napl/resources}.
#'
#' @seealso \code{\link{get_wang_prob_atlas}} for the full per-area probability
#'   volumes, \code{\link{get_visfatlas}} for a volumetric visual-cortex atlas,
#'   \code{\link{get_visual_atlas}} for cytoarchitectonic V1-V5.
#'
#' @examples
#' \dontrun{
#' # Wang 2015 visual topography atlas on the fsaverage inflated surface
#' wang <- get_wang_atlas(surf = "inflated")
#' wang$labels
#' get_roi(wang, label = "hV4")
#' }
#'
#' @importFrom methods new
#' @importFrom grDevices rainbow col2rgb
#' @export
get_wang_atlas <- function(surf = c("inflated", "pial", "white", "midthickness"),
                           space = "fsaverage",
                           use_cache = TRUE) {
  surf <- match.arg(surf)
  space <- match.arg(space, "fsaverage")

  lh <- .wang_surface_hemi("lh", surf = surf, use_cache = use_cache)
  rh <- .wang_surface_hemi("rh", surf = surf, use_cache = use_cache)

  # Flat region catalogue: the same 25 areas in each hemisphere, distinguished
  # by the `hemi` vector (the per-hemisphere surfaces remain self-contained).
  areas <- .wang_visual_labels()$label
  labels <- c(areas, areas)
  ids <- seq_along(labels)
  hemi <- c(rep("left", length(areas)), rep("right", length(areas)))

  pal <- .wang_visual_colors()
  cmap <- as.data.frame(rbind(pal, pal))

  ref <- new_atlas_ref(
    family = "wang",
    model = "Wang2015",
    representation = "surface",
    template_space = "fsaverage",
    coord_space = get_surface_coordinate_space("fsaverage"),
    density = "164k",
    provenance = "https://github.com/noahbenson/neuropythy",
    source = "neuropythy_fsaverage",
    lineage = paste(
      "Wang 2015 maximum-probability map (wang15_mplbl) on fsaverage,",
      "bundled with neuropythy."
    ),
    confidence = "high",
    notes = "Maximum-probability surface labels; 25 topographic areas per hemisphere."
  )

  artifacts <- dplyr::bind_rows(
    .new_atlas_artifact(
      role = "label_overlay_lh",
      family = "wang",
      model = "Wang2015",
      source_name = "neuropythy",
      source_url = "https://github.com/noahbenson/neuropythy",
      source_ref = "lh.wang15_mplbl.v1_0.mgz",
      citation_doi = "10.1093/cercor/bhu277",
      file_name = "lh.wang15_mplbl.v1_0.mgz",
      template_space = "fsaverage",
      coord_space = get_surface_coordinate_space("fsaverage"),
      density = "164k",
      hemi = "left",
      lineage = "neuropythy-bundled Wang 2015 MPM overlay.",
      confidence = "high"
    ),
    .new_atlas_artifact(
      role = "label_overlay_rh",
      family = "wang",
      model = "Wang2015",
      source_name = "neuropythy",
      source_url = "https://github.com/noahbenson/neuropythy",
      source_ref = "rh.wang15_mplbl.v1_0.mgz",
      citation_doi = "10.1093/cercor/bhu277",
      file_name = "rh.wang15_mplbl.v1_0.mgz",
      template_space = "fsaverage",
      coord_space = get_surface_coordinate_space("fsaverage"),
      density = "164k",
      hemi = "right",
      lineage = "neuropythy-bundled Wang 2015 MPM overlay.",
      confidence = "high"
    ),
    .new_atlas_artifact(
      role = "geometry",
      family = "wang",
      model = "Wang2015",
      source_name = "TemplateFlow",
      source_url = "https://www.templateflow.org",
      template_space = "fsaverage",
      coord_space = get_surface_coordinate_space("fsaverage"),
      density = "164k",
      lineage = paste0("TemplateFlow fsaverage surface geometry (", surf, ")."),
      confidence = "high"
    )
  )

  history <- .new_atlas_history(
    action = "load",
    representation = "surface",
    from_template_space = "fsaverage",
    to_template_space = "fsaverage",
    from_coord_space = get_surface_coordinate_space("fsaverage"),
    to_coord_space = get_surface_coordinate_space("fsaverage"),
    status = "available",
    confidence = "high",
    details = paste0("Loaded Wang 2015 fsaverage surface atlas (", surf, ").")
  )

  new_surfatlas(
    name = "Wang2015 visual topography (fsaverage)",
    lh_atlas = lh,
    rh_atlas = rh,
    ids = ids,
    labels = labels,
    orig_labels = labels,
    hemi = hemi,
    cmap = cmap,
    surf_type = surf,
    surface_space = "fsaverage",
    subclass = "wang",
    ref = ref,
    artifacts = artifacts,
    history = history
  )
}


#' Canonical Wang (2015) ROI label table
#'
#' The 25 topographic areas in the order used by the ProbAtlas / neuropythy
#' maximum-probability labels (overlay value `k` -> row `k`).
#'
#' @keywords internal
#' @noRd
.wang_visual_labels <- function() {
  tibble::tibble(
    id = 1:25,
    label = c(
      "V1v", "V1d", "V2v", "V2d", "V3v", "V3d", "hV4",
      "VO1", "VO2", "PHC1", "PHC2", "TO2", "TO1", "LO2", "LO1",
      "V3B", "V3A", "IPS0", "IPS1", "IPS2", "IPS3", "IPS4", "IPS5",
      "SPL1", "FEF"
    )
  )
}


#' Deterministic 25-colour palette (RGB matrix) for Wang areas.
#'
#' @keywords internal
#' @noRd
.wang_visual_colors <- function() {
  cols <- grDevices::rainbow(25)
  rgb <- t(grDevices::col2rgb(cols))
  colnames(rgb) <- c("red", "green", "blue")
  rgb
}


#' Resolve (download + cache) a Wang fsaverage overlay file.
#'
#' @keywords internal
#' @noRd
.wang_mgz_path <- function(hemi, use_cache = TRUE) {
  hemi <- match.arg(hemi, c("lh", "rh"))
  fname <- paste0(hemi, ".wang15_mplbl.v1_0.mgz")

  cache_dir <- .neuroatlas_cache_dir("wang")
  fpath <- file.path(cache_dir, fname)
  if (use_cache && file.exists(fpath)) {
    return(fpath)
  }

  url <- paste0(
    "https://raw.githubusercontent.com/noahbenson/neuropythy/master/",
    "neuropythy/lib/data/fsaverage/surf/", fname
  )

  tmp <- tempfile(fileext = ".mgz")
  on.exit(unlink(tmp), add = TRUE)
  .neuroatlas_download(
    url = url,
    dest = tmp,
    min_size = 4000L,
    description = paste0("Wang 2015 overlay (", fname, ")")
  )

  ok <- file.copy(tmp, fpath, overwrite = TRUE)
  if (!ok) {
    cli::cli_abort(
      "Failed to cache Wang overlay file at {.path {fpath}}.",
      class = c("neuroatlas_error_cache", "neuroatlas_error")
    )
  }
  fpath
}


#' Build a single-hemisphere Wang \code{LabeledNeuroSurface}.
#'
#' @keywords internal
#' @noRd
.wang_surface_hemi <- function(hemi,
                               surf = c("inflated", "pial", "white", "midthickness"),
                               use_cache = TRUE) {
  hemi <- match.arg(hemi, c("lh", "rh"))
  surf <- match.arg(surf)

  mgz_path <- .wang_mgz_path(hemi, use_cache = use_cache)
  vertex_labels <- .read_mgh_data(mgz_path)
  vertex_labels <- as.integer(round(vertex_labels))

  hemi_tf <- if (hemi == "lh") "L" else "R"
  surf_path <- tryCatch(
    get_surface_template(
      template_id = "fsaverage",
      surface_type = surf,
      hemi = hemi_tf,
      density = "164k",
      load_as_path = TRUE
    ),
    error = function(e) NULL
  )

  geom <- NULL
  if (!is.null(surf_path) && file.exists(surf_path)) {
    geom <- tryCatch(
      neurosurf::read_surf_geometry(surf_path),
      error = function(e) NULL
    )
    if (is.null(geom) && requireNamespace("gifti", quietly = TRUE)) {
      geom <- tryCatch({
        gii <- gifti::readgii(surf_path)
        hemi_label <- if (hemi == "lh") "left" else "right"
        neurosurf::SurfaceGeometry(
          vert = gii$data[[1]], faces = gii$data[[2]], hemi = hemi_label
        )
      }, error = function(e) NULL)
    }
  }

  if (is.null(geom)) {
    cli::cli_abort(
      c(
        "Could not obtain fsaverage (164k) surface geometry for the Wang atlas.",
        "i" = "The Wang atlas requires TemplateFlow fsaverage geometry (see {.fn get_surface_template}).",
        "i" = "Check your TemplateFlow / reticulate setup."
      ),
      class = c("neuroatlas_error_surface_geometry", "neuroatlas_error")
    )
  }

  n_vertices <- ncol(geom@mesh$vb)
  if (length(vertex_labels) != n_vertices) {
    cli::cli_abort(
      c(
        "Vertex count mismatch between Wang overlay and fsaverage geometry.",
        "x" = "Overlay has {length(vertex_labels)} vertices; geometry has {n_vertices}.",
        "i" = "Wang 2015 labels are defined on fsaverage (164k = 163842 vertices)."
      ),
      class = c("neuroatlas_error_surface_geometry", "neuroatlas_error")
    )
  }

  # Surface colour table: index 1 is the unlabeled background ("???"),
  # indices 2..26 are the 25 topographic areas. Per-vertex codes are the
  # overlay value + 1 so that overlay value 0 maps to background.
  labs <- c("???", .wang_visual_labels()$label)
  cols <- c("#7F7F7F", grDevices::rainbow(25))

  methods::new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = as.integer(seq_len(n_vertices)),
    data = as.numeric(vertex_labels + 1L),
    labels = as.character(labs),
    cols = as.character(cols)
  )
}


#' Minimal reader for FreeSurfer MGH/MGZ surface overlays.
#'
#' Reads the per-element data from an uncompressed (\code{.mgh}) or
#' gzip-compressed (\code{.mgz}) FreeSurfer volume/overlay file without
#' requiring \pkg{freesurferformats}. The fixed 284-byte header is parsed for
#' dimensions and data type; the data block is returned as a numeric vector
#' (length = width * height * depth * nframes).
#'
#' @param path Path to an `.mgh` or `.mgz` file.
#' @return A numeric vector of the file's data values.
#' @keywords internal
#' @noRd
.read_mgh_data <- function(path) {
  con <- if (grepl("\\.mgz$", path, ignore.case = TRUE)) {
    gzfile(path, "rb")
  } else {
    file(path, "rb")
  }
  on.exit(close(con))

  header <- readBin(con, "integer", n = 7L, size = 4L, endian = "big")
  width <- header[2]
  height <- header[3]
  depth <- header[4]
  nframes <- header[5]
  type <- header[6]
  n <- as.numeric(width) * height * depth * nframes

  # Data begins at byte offset 284; 7 ints (28 bytes) already consumed.
  invisible(readBin(con, "raw", n = 284L - 28L))

  dat <- switch(
    as.character(type),
    "0" = readBin(con, "integer", n = n, size = 1L, signed = FALSE, endian = "big"),
    "1" = readBin(con, "integer", n = n, size = 4L, endian = "big"),
    "3" = readBin(con, "double", n = n, size = 4L, endian = "big"),
    "4" = readBin(con, "integer", n = n, size = 2L, endian = "big"),
    cli::cli_abort(
      "Unsupported MGH data type code {.val {type}} in {.path {path}}.",
      class = c("neuroatlas_error_mgh", "neuroatlas_error")
    )
  )
  as.numeric(dat)
}


#' Wang (2015) Full Per-Area Probability Volumes (Princeton ProbAtlas_v4)
#'
#' @description
#' Resolve (and optionally load) the full per-area probability maps and
#' maximum-probability volumes from the original Princeton ProbAtlas_v4
#' distribution of the Wang et al. (2015) visual topography atlas. These are
#' the volumetric counterparts to the surface labels returned by
#' \code{\link{get_wang_atlas}()}: for each of the 25 topographic areas there is
#' a continuous probability map (\code{perc_VTPM_vol_roi<n>_<hemi>.nii.gz}) in
#' MNI volume space, plus a maximum-probability summary
#' (\code{maxprob_vol_<hemi>.nii.gz}).
#'
#' @details
#' The Princeton server (\code{scholar.princeton.edu} / \code{napl}) blocks
#' automated downloads of the binary archive (it returns HTTP 403 to scripted
#' requests), so this function does \strong{not} download the data. Instead it
#' defaults to \code{path_only = TRUE} and returns a manifest describing the
#' expected files, the manual download URL, and the canonical ROI labels.
#'
#' To work with the actual volumes, download \code{ProbAtlas_v4.zip} once in a
#' browser from the Princeton resources page, unzip it, and pass the resulting
#' directory (or its \code{subj_vol_all} subfolder) via \code{prob_dir}. With a
#' valid \code{prob_dir} you can set \code{path_only = FALSE} to read the
#' requested volumes as \code{NeuroVol} objects.
#'
#' @param prob_dir Optional path to a locally-extracted ProbAtlas_v4 directory
#'   (the folder containing \code{subj_vol_all}, or that subfolder itself).
#'   When supplied, file paths are resolved and existence-checked.
#' @param image One of \code{"probability"} (per-area \code{perc_VTPM} maps,
#'   the default) or \code{"maxprob"} (the maximum-probability summary volume).
#' @param hemi One of \code{"both"} (default), \code{"lh"}, or \code{"rh"}.
#' @param rois Optional subset of areas, given as labels (e.g.
#'   \code{c("V1v", "hV4")}) or integer ids (1-25). \code{NULL} selects all 25.
#' @param path_only Logical. When \code{TRUE} (default), return a manifest of
#'   paths/metadata without reading image data. When \code{FALSE}, read the
#'   resolved volumes (requires a valid \code{prob_dir}).
#' @param use_cache Logical. Also look for files under the neuroatlas Wang cache
#'   directory when \code{prob_dir} is not supplied.
#'
#' @return
#' When \code{path_only = TRUE}, an object of class \code{wang_prob_paths}: a
#' list with the requested \code{image}/\code{hemi}, the manual download
#' \code{zip_url}, a \code{files} tibble (\code{id}, \code{label}, \code{hemi},
#' \code{member}, \code{path}, \code{exists}), and the canonical \code{labels}.
#' When \code{path_only = FALSE}, a \code{wang_prob_volumes} list whose
#' \code{volumes} element holds the loaded \code{NeuroVol} objects.
#'
#' @references
#' Wang, L., Mruczek, R. E. B., Arcaro, M. J., & Kastner, S. (2015).
#' Probabilistic Maps of Visual Topography in Human Cortex. Cerebral Cortex,
#' 25(10), 3911-3931. \doi{10.1093/cercor/bhu277}
#'
#' @source \url{https://napl.scholar.princeton.edu/resources}
#'
#' @seealso \code{\link{get_wang_atlas}} for the fsaverage surface atlas.
#'
#' @examples
#' \dontrun{
#' # Manifest only (no download): see which files are needed and where to get them
#' manifest <- get_wang_prob_atlas()
#' manifest
#' head(manifest$files)
#'
#' # After manually downloading + unzipping ProbAtlas_v4.zip:
#' paths <- get_wang_prob_atlas(prob_dir = "~/atlases/ProbAtlas_v4")
#' v1v_lh <- get_wang_prob_atlas(prob_dir = "~/atlases/ProbAtlas_v4",
#'                               rois = "V1v", hemi = "lh", path_only = FALSE)
#' }
#'
#' @importFrom neuroim2 read_vol
#' @export
get_wang_prob_atlas <- function(prob_dir = NULL,
                                image = c("probability", "maxprob"),
                                hemi = c("both", "lh", "rh"),
                                rois = NULL,
                                path_only = TRUE,
                                use_cache = TRUE) {
  image <- match.arg(image)
  hemi <- match.arg(hemi)

  files <- .wang_prob_files(image, hemi, rois)

  subj_vol_all <- NULL
  if (!is.null(prob_dir)) {
    subj_vol_all <- .wang_prob_resolve_subjvol(prob_dir)
  }
  if (is.null(subj_vol_all) && isTRUE(use_cache)) {
    subj_vol_all <- .wang_prob_resolve_subjvol(.neuroatlas_cache_dir("wang"))
  }

  files$path <- vapply(files$member, function(m) {
    .wang_prob_resolve_file(subj_vol_all, m)
  }, character(1))
  files$exists <- !is.na(files$path)

  zip_url <- paste0(
    "https://napl.scholar.princeton.edu/sites/g/files/toruqf3751/files/",
    "napl/files/ProbAtlas_v4.zip"
  )

  if (isTRUE(path_only)) {
    ret <- list(
      dataset = "Wang2015 ProbAtlas_v4 (probability volumes)",
      image = image,
      hemi = hemi,
      zip_url = zip_url,
      resources_url = "https://napl.scholar.princeton.edu/resources",
      prob_dir = prob_dir %||% NA_character_,
      subj_vol_all = subj_vol_all %||% NA_character_,
      labels = .wang_visual_labels(),
      files = tibble::as_tibble(files),
      note = paste(
        "Princeton blocks automated downloads (HTTP 403).",
        "Download ProbAtlas_v4.zip in a browser, unzip it, and pass the",
        "folder via 'prob_dir' to resolve/load these volumes."
      )
    )
    class(ret) <- c("wang_prob_paths", "list")
    return(ret)
  }

  if (is.null(subj_vol_all) || any(!files$exists)) {
    missing_members <- files$member[!files$exists]
    cli::cli_abort(
      c(
        "Could not resolve Wang probability volumes for {.code path_only = FALSE}.",
        "i" = "Download {.url {zip_url}} in a browser, unzip it, and pass {.arg prob_dir}.",
        "x" = if (is.null(subj_vol_all)) {
          "No {.path subj_vol_all} directory was found."
        } else {
          "Missing file{?s}: {.path {missing_members}}."
        }
      ),
      class = c("neuroatlas_error_wang_prob", "neuroatlas_error")
    )
  }

  vols <- lapply(files$path, neuroim2::read_vol)
  names(vols) <- if (identical(image, "maxprob")) {
    files$hemi
  } else {
    paste0(files$label, "_", files$hemi)
  }

  ret <- list(
    dataset = "Wang2015 ProbAtlas_v4 (probability volumes)",
    image = image,
    hemi = hemi,
    subj_vol_all = subj_vol_all,
    labels = .wang_visual_labels(),
    files = tibble::as_tibble(files),
    volumes = vols
  )
  class(ret) <- c("wang_prob_volumes", "list")
  ret
}


#' Build the Wang ProbAtlas_v4 volume file manifest.
#'
#' @keywords internal
#' @noRd
.wang_prob_files <- function(image, hemi, rois) {
  labs <- .wang_visual_labels()

  if (!is.null(rois)) {
    if (is.character(rois)) {
      idx <- match(rois, labs$label)
      if (anyNA(idx)) {
        cli::cli_abort(
          "Unknown Wang ROI label{?s}: {.val {rois[is.na(idx)]}}.",
          class = c("neuroatlas_error_wang_prob", "neuroatlas_error")
        )
      }
    } else {
      idx <- match(as.integer(rois), labs$id)
      if (anyNA(idx)) {
        cli::cli_abort(
          "Wang ROI ids must be integers in 1-25.",
          class = c("neuroatlas_error_wang_prob", "neuroatlas_error")
        )
      }
    }
    labs <- labs[idx, , drop = FALSE]
  }

  hemis <- if (identical(hemi, "both")) c("lh", "rh") else hemi

  if (identical(image, "maxprob")) {
    data.frame(
      id = NA_integer_,
      label = "maxprob",
      hemi = hemis,
      member = file.path("subj_vol_all",
                         paste0("maxprob_vol_", hemis, ".nii.gz")),
      stringsAsFactors = FALSE
    )
  } else {
    grid <- expand.grid(row = seq_len(nrow(labs)), hemi = hemis,
                        stringsAsFactors = FALSE)
    data.frame(
      id = labs$id[grid$row],
      label = labs$label[grid$row],
      hemi = grid$hemi,
      member = file.path(
        "subj_vol_all",
        paste0("perc_VTPM_vol_roi", labs$id[grid$row], "_", grid$hemi, ".nii.gz")
      ),
      stringsAsFactors = FALSE
    )
  }
}


#' Locate a `subj_vol_all` directory under a user-supplied ProbAtlas path.
#'
#' @keywords internal
#' @noRd
.wang_prob_resolve_subjvol <- function(prob_dir) {
  if (is.null(prob_dir) || !nzchar(prob_dir) || !dir.exists(prob_dir)) {
    return(NULL)
  }
  if (identical(basename(normalizePath(prob_dir)), "subj_vol_all")) {
    return(normalizePath(prob_dir))
  }
  direct <- file.path(prob_dir, "subj_vol_all")
  if (dir.exists(direct)) {
    return(normalizePath(direct))
  }
  dirs <- list.dirs(prob_dir, recursive = TRUE, full.names = TRUE)
  hits <- dirs[basename(dirs) == "subj_vol_all"]
  if (length(hits) > 0L) normalizePath(hits[1]) else NULL
}


#' Resolve a single ProbAtlas member file (tolerant of .nii / .nii.gz).
#'
#' @keywords internal
#' @noRd
.wang_prob_resolve_file <- function(subj_vol_all, member) {
  if (is.null(subj_vol_all)) {
    return(NA_character_)
  }
  base <- file.path(subj_vol_all, basename(member))
  candidates <- c(base, sub("\\.gz$", "", base))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0L) normalizePath(hit[1]) else NA_character_
}


#' @rdname get_wang_prob_atlas
#' @param x A \code{wang_prob_paths} object.
#' @param ... Unused.
#' @export
print.wang_prob_paths <- function(x, ...) {
  cat("<wang_prob_paths>\n")
  cat("  dataset:  ", x$dataset, "\n", sep = "")
  cat("  image:    ", x$image, " | hemi: ", x$hemi, "\n", sep = "")
  n_present <- sum(x$files$exists)
  cat("  files:    ", nrow(x$files), " (", n_present, " present locally)\n",
      sep = "")
  if (is.na(x$subj_vol_all)) {
    cat("  prob_dir: <not set> -- download manually:\n")
    cat("            ", x$zip_url, "\n", sep = "")
  } else {
    cat("  subj_vol_all: ", x$subj_vol_all, "\n", sep = "")
  }
  cat("  note:     ", x$note %||% "", "\n", sep = "")
  invisible(x)
}
