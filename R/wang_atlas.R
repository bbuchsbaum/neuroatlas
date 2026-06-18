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
#' required (mirroring \code{\link{glasser_surf}}). The volumetric counterparts
#' (maximum-probability and per-area probability maps in MNI space) are served
#' by \code{\link{get_wang_prob_atlas}}.
#'
#' TemplateFlow does not currently distribute an \emph{inflated} \code{fsaverage}
#' mesh, so the available surfaces are \code{"midthickness"} (default),
#' \code{"pial"}, and \code{"white"}.
#'
#' @param surf Surface type. One of \code{"midthickness"} (default),
#'   \code{"pial"}, or \code{"white"}. (TemplateFlow does not provide an
#'   inflated fsaverage surface.)
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
#' # Wang 2015 visual topography atlas on the fsaverage midthickness surface
#' wang <- get_wang_atlas(surf = "midthickness")
#' wang$labels
#' # Extract an area as ROISurface objects (one per hemisphere)
#' get_roi(wang, label = "hV4")
#' get_roi(wang, label = "V1v", hemi = "left")
#' }
#'
#' @importFrom methods new
#' @importFrom grDevices rainbow col2rgb
#' @export
get_wang_atlas <- function(surf = c("midthickness", "pial", "white"),
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
                               surf = c("midthickness", "pial", "white"),
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

  # Encode per-vertex codes to match the package surfatlas convention shared by
  # plot_brain() and the other surface atlases (see schaefer_surf): 0 is the
  # unlabeled background and area codes equal the global atlas ids -- 1..25 for
  # the left hemisphere and 26..50 for the right. The raw overlay uses 0 for
  # background and 1..25 for the areas within each hemisphere.
  id_offset <- if (hemi == "rh") 25L else 0L
  codes <- ifelse(vertex_labels == 0L, 0L, vertex_labels + id_offset)

  labs <- .wang_visual_labels()$label
  cols <- grDevices::rainbow(25)

  methods::new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = as.integer(seq_len(n_vertices)),
    data = as.numeric(codes),
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
#' On first load the volumes (~0.7 MB) are downloaded from the neuroatlas
#' GitHub release and cached under the neuroatlas Wang cache directory, after
#' which they resolve offline. They originate from the Princeton ProbAtlas_v4
#' distribution (Wang et al. 2015), whose original host
#' (\code{napl.scholar.princeton.edu}) blocks scripted downloads and is no
#' longer reliably available; the archive carried no licence and the maps are
#' widely redistributed as an open-science resource, so they are re-hosted on
#' the package's own release for programmatic access.
#'
#' To use a local copy instead (e.g. an updated release), download/unzip
#' \code{ProbAtlas_v4} and pass the resulting directory (or its
#' \code{subj_vol_all} subfolder) via \code{prob_dir}; it takes precedence and
#' is never mixed with the cache or download. Set \code{path_only = FALSE} to
#' read the requested volumes as \code{NeuroVol} objects.
#'
#' The resolution order on a load is \code{prob_dir} -> cache -> download. A
#' read-only manifest (\code{path_only = TRUE}) resolves from
#' \code{prob_dir}/cache only and never downloads or writes to the cache.
#' When \code{path_only = FALSE} and \code{use_cache = TRUE}, volumes read from
#' a user-supplied \code{prob_dir} are also copied (best-effort, atomically)
#' into the cache.
#'
#' Note on naming: the volume coding (\code{ROIfiles_Labeling.txt}) labels areas
#' 12/13 as \code{MST}/\code{hMT}, which correspond to \code{TO2}/\code{TO1} in
#' the surface (\pkg{neuropythy}) naming used by \code{\link{get_wang_atlas}()};
#' the numeric ids are identical.
#'
#' @param prob_dir Optional path to a locally-extracted ProbAtlas_v4 directory
#'   (the folder containing \code{subj_vol_all}, or that subfolder itself). When
#'   supplied it is used exclusively; otherwise the cached/downloaded data is
#'   used.
#' @param image One of \code{"probability"} (per-area \code{perc_VTPM} maps,
#'   the default) or \code{"maxprob"} (the maximum-probability summary volume).
#' @param hemi One of \code{"both"} (default), \code{"lh"}, or \code{"rh"}.
#' @param rois Optional subset of areas, given as labels (e.g.
#'   \code{c("V1v", "hV4")}) or integer ids (1-25). \code{NULL} selects all 25.
#' @param path_only Logical. When \code{TRUE} (default), return a manifest of
#'   paths/metadata without downloading or reading image data. When
#'   \code{FALSE}, read the resolved volumes as \code{NeuroVol} objects,
#'   downloading them to the cache on first use if needed.
#' @param use_cache Logical. Look for (and download into) the neuroatlas Wang
#'   cache directory when \code{prob_dir} is not supplied. \code{FALSE} forces a
#'   fresh download on load.
#'
#' @return
#' When \code{path_only = TRUE}, an object of class \code{wang_prob_paths}: a
#' list with the requested \code{image}/\code{hemi}, the \code{resources_url}
#' and \code{download_url}, a \code{files} tibble (\code{id}, \code{label},
#' \code{hemi}, \code{member}, \code{path}, \code{exists}), and the canonical
#' \code{labels}. When \code{path_only = FALSE}, a \code{wang_prob_volumes} list
#' whose \code{volumes} element holds the loaded \code{NeuroVol} objects.
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
#' # Manifest only (no download): what is available and where it comes from
#' manifest <- get_wang_prob_atlas()
#' manifest
#' head(manifest$files)
#'
#' # Load the maximum-probability volumes (downloaded + cached on first use)
#' wp <- get_wang_prob_atlas(image = "maxprob", path_only = FALSE)
#'
#' # Load a single area's probability map
#' v1v_lh <- get_wang_prob_atlas(rois = "V1v", hemi = "lh", path_only = FALSE)
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

  user_subj_vol_all <- NULL
  if (!is.null(prob_dir)) {
    user_subj_vol_all <- .wang_prob_resolve_subjvol(prob_dir)
    if (is.null(user_subj_vol_all)) {
      # An explicit prob_dir must resolve; never silently fall back to the
      # cache or bundled data when the user pointed somewhere specific.
      cli::cli_abort(
        c(
          "No {.path subj_vol_all} directory found under the supplied {.arg prob_dir}.",
          "x" = "{.path {prob_dir}}",
          "i" = paste(
            "Point {.arg prob_dir} at an extracted ProbAtlas_v4 folder (or its",
            "{.path subj_vol_all} subfolder), or omit it to download/cache it."
          )
        ),
        class = c("neuroatlas_error_wang_prob", "neuroatlas_error")
      )
    }
  }

  # Build the directory search order. An explicit prob_dir is authoritative
  # (used alone). Otherwise read from the cache; on an actual load, download
  # the volumes into the cache when they are not already present.
  resolve_paths <- function(dirs) {
    vapply(files$member, function(m) {
      for (d in dirs) {
        p <- .wang_prob_resolve_file(d, m)
        if (!is.na(p)) return(p)
      }
      NA_character_
    }, character(1))
  }

  if (!is.null(user_subj_vol_all)) {
    cand_dirs <- user_subj_vol_all
  } else {
    cand_dirs <- character(0)
    if (isTRUE(use_cache)) {
      # Read-only lookup: never create the cache dir merely to inspect it, so a
      # manifest (path_only = TRUE) call stays free of home-directory writes.
      cd <- .wang_prob_resolve_subjvol(
        .neuroatlas_cache_dir("wang", create = FALSE)
      )
      if (!is.null(cd)) cand_dirs <- c(cand_dirs, cd)
    }
  }

  files$path <- resolve_paths(cand_dirs)

  # On a load (not a manifest) without an explicit prob_dir, fetch the volumes
  # from the neuroatlas GitHub release into the cache if any are missing.
  if (!isTRUE(path_only) && is.null(user_subj_vol_all) && any(is.na(files$path))) {
    dl <- .wang_prob_download(use_cache = use_cache)
    if (!is.null(dl)) {
      cand_dirs <- unique(c(cand_dirs, dl))
      files$path <- resolve_paths(cand_dirs)
    }
  }
  files$exists <- !is.na(files$path)
  subj_vol_all <- if (length(cand_dirs) >= 1L) cand_dirs[[1]] else NULL

  resources_url <- "https://napl.scholar.princeton.edu/resources"

  if (isTRUE(path_only)) {
    ret <- list(
      dataset = "Wang2015 ProbAtlas_v4 (probability volumes)",
      image = image,
      hemi = hemi,
      resources_url = resources_url,
      download_url = .wang_prob_release_url(),
      prob_dir = prob_dir %||% NA_character_,
      subj_vol_all = subj_vol_all %||% NA_character_,
      labels = .wang_visual_labels(),
      files = tibble::as_tibble(files),
      note = paste(
        "Volumes are downloaded on first load from the neuroatlas GitHub",
        "release and cached; pass path_only = FALSE to fetch them, or supply",
        "'prob_dir' to use a local copy.",
        "Original source: Wang et al. (2015) ProbAtlas_v4,",
        resources_url
      )
    )
    class(ret) <- c("wang_prob_paths", "list")
    return(ret)
  }

  if (is.null(subj_vol_all) || any(!files$exists)) {
    missing_members <- files$member[!files$exists]
    rel_url <- .wang_prob_release_url()
    cli::cli_abort(
      c(
        "Could not resolve Wang probability volumes for {.code path_only = FALSE}.",
        "i" = "They are fetched from {.url {rel_url}} on first load; check your network connection.",
        "i" = "Alternatively pass {.arg prob_dir} pointing at a local ProbAtlas_v4 directory.",
        "x" = if (is.null(subj_vol_all)) {
          "No {.path subj_vol_all} directory was found."
        } else {
          "Missing file{?s}: {.path {missing_members}}."
        }
      ),
      class = c("neuroatlas_error_wang_prob", "neuroatlas_error")
    )
  }

  vols <- tryCatch(
    lapply(files$path, neuroim2::read_vol),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to read a Wang volume file.",
          "x" = conditionMessage(e),
          "i" = "A cached file may be corrupt; re-run with {.code use_cache = FALSE} to refetch it."
        ),
        class = c("neuroatlas_error_wang_prob", "neuroatlas_error"),
        parent = e
      )
    }
  )
  names(vols) <- if (identical(image, "maxprob")) {
    files$hemi
  } else {
    paste0(files$label, "_", files$hemi)
  }

  # Persist the successfully-read volumes into the neuroatlas cache so later
  # calls resolve offline without `prob_dir`, mirroring the other downloadable
  # volume atlases (e.g. visfAtlas). Seeding runs only after a successful read
  # from a user-supplied directory, copies atomically, and is best-effort: a
  # failed copy never disrupts the current load.
  if (isTRUE(use_cache) && !is.null(user_subj_vol_all)) {
    try(.wang_prob_cache_seed(files$path), silent = TRUE)
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


#' Stable download URL for the bundled-and-rehosted Wang volume archive.
#' @keywords internal
#' @noRd
.wang_prob_release_url <- function() {
  paste0(
    "https://github.com/bbuchsbaum/neuroatlas/releases/download/",
    "wang-probatlas-v4/wang_probatlas_v4_volumes.tar.gz"
  )
}

#' Download + extract the Wang ProbAtlas_v4 volumes into the neuroatlas cache.
#'
#' Fetches the (~0.7 MB) volume archive from the neuroatlas GitHub release and
#' unpacks \code{subj_vol_all} into \code{<cache>/wang/subj_vol_all/}. The
#' original Princeton host blocks scripted downloads, so the volumes are
#' re-hosted on the package's own release for programmatic access.
#'
#' @param use_cache Logical. When \code{TRUE}, reuse cached files if already
#'   complete (the caller checks this); the download always targets the cache.
#' @return The cache \code{subj_vol_all} path, or \code{NULL}.
#' @keywords internal
#' @noRd
.wang_prob_download <- function(use_cache = TRUE) {
  cache_dir <- .neuroatlas_cache_dir("wang")
  cache_subj <- file.path(cache_dir, "subj_vol_all")
  tar_path <- file.path(cache_dir, "wang_probatlas_v4_volumes.tar.gz")

  .neuroatlas_download(
    url = .wang_prob_release_url(),
    dest = tar_path,
    min_size = 100000L,
    description = "Wang ProbAtlas_v4 volumes"
  )
  on.exit(unlink(tar_path), add = TRUE)

  exdir <- tempfile("wang-extract-")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  if (!identical(utils::untar(tar_path, exdir = exdir), 0L)) {
    cli::cli_abort(
      "Failed to unpack the downloaded Wang volume archive.",
      class = c("neuroatlas_error_wang_prob", "neuroatlas_error")
    )
  }

  src_subj <- .wang_prob_resolve_subjvol(exdir)
  if (is.null(src_subj)) {
    cli::cli_abort(
      "Downloaded Wang archive did not contain a {.path subj_vol_all} directory.",
      class = c("neuroatlas_error_wang_prob", "neuroatlas_error")
    )
  }

  dir.create(cache_subj, recursive = TRUE, showWarnings = FALSE)
  src_files <- list.files(src_subj, full.names = TRUE)
  copied <- file.copy(src_files, cache_subj, overwrite = TRUE)
  if (!all(copied)) {
    cli::cli_abort(
      "Failed to populate the Wang cache from the downloaded archive.",
      class = c("neuroatlas_error_wang_prob", "neuroatlas_error")
    )
  }
  normalizePath(cache_subj)
}

#' Persist successfully-read ProbAtlas volume files into the Wang cache.
#'
#' Copies the given (already-resolved, already-read) volume files into
#' \code{<cache>/wang/subj_vol_all/} so subsequent calls resolve offline
#' without \code{prob_dir}. Copies are atomic (temp file then
#' \code{file.rename()}) to avoid leaving truncated NIfTIs behind, and a
#' cached file whose size disagrees with the source is replaced so a poisoned
#' cache entry can be repaired. Files already living in the cache are skipped
#' (no self-copy). Best-effort: individual copy failures are ignored.
#'
#' @param file_paths Character vector of resolved source file paths.
#' @return The cache \code{subj_vol_all} path (invisibly), or \code{NULL}.
#' @keywords internal
#' @noRd
.wang_prob_cache_seed <- function(file_paths) {
  file_paths <- file_paths[!is.na(file_paths) & file.exists(file_paths)]
  if (length(file_paths) == 0L) {
    return(invisible(NULL))
  }
  cache_subj <- file.path(.neuroatlas_cache_dir("wang"), "subj_vol_all")
  dir.create(cache_subj, showWarnings = FALSE, recursive = TRUE)
  cache_norm <- normalizePath(cache_subj, mustWork = FALSE)

  for (src in file_paths) {
    # Skip files that already live in the cache (no self-copy).
    if (identical(normalizePath(dirname(src), mustWork = FALSE), cache_norm)) {
      next
    }
    dest <- file.path(cache_subj, basename(src))
    src_size <- file.size(src)
    # A good copy already exists -> nothing to do; otherwise (re)write it.
    if (file.exists(dest) && isTRUE(file.size(dest) == src_size)) {
      next
    }
    tmp <- tempfile(tmpdir = cache_subj, fileext = ".part")
    ok <- file.copy(src, tmp, overwrite = TRUE)
    if (isTRUE(ok) && isTRUE(file.size(tmp) == src_size)) {
      # Publish atomically. Remove any (corrupt) existing file first so the
      # rename succeeds cross-platform; fall back to copy across devices.
      if (file.exists(dest)) unlink(dest)
      if (!isTRUE(file.rename(tmp, dest))) {
        file.copy(tmp, dest, overwrite = TRUE)
        unlink(tmp)
      }
    } else {
      unlink(tmp)
    }
  }
  invisible(cache_subj)
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
    cat("  subj_vol_all: <not cached> -- downloaded on load from:\n")
    cat("            ", x$download_url %||% x$resources_url %||% "", "\n", sep = "")
  } else {
    cat("  subj_vol_all: ", x$subj_vol_all, "\n", sep = "")
  }
  cat("  note:     ", x$note %||% "", "\n", sep = "")
  invisible(x)
}
