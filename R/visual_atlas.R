#' Early Visual Cortex Atlas (V1-V5, cytoarchitectonic)
#'
#' @description
#' A convenience loader that extracts the early visual areas from the
#' Julich-Brain cytoarchitectonic atlas and relabels them as \code{V1}-\code{V5}
#' per hemisphere. This provides a compact, probabilistically-derived
#' early-visual parcellation in MNI volume space without the surrounding
#' whole-brain regions.
#'
#' @details
#' The source regions are the Julich-Brain maximum-probability labels
#' \code{GM Visual cortex V1 BA17}, \code{V2 BA18}, \code{V3V}, \code{V4}, and
#' \code{V5} (left/right), loaded via \code{\link{get_julich_brain_atlas}()}.
#' Note that the Julich atlas defines only the ventral subdivision of V3
#' (\code{V3V}) and a single \code{V4}/\code{V5} region per hemisphere.
#'
#' For the topographic surface atlas with dorsal/ventral subdivisions and
#' \code{hV4}, see \code{\link{get_wang_atlas}}; for a volumetric functional
#' atlas, see \code{\link{get_visfatlas}}.
#'
#' @param outspace Optional \code{NeuroSpace} object to resample the atlas into.
#' @param smooth Logical. Whether to smooth parcel boundaries when resampling.
#' @param resolution Optional Julich-Brain resolution (e.g. \code{"1mm"} or
#'   \code{"2mm"}); passed to \code{\link{get_julich_brain_atlas}()}.
#' @param fsl_dir FSL installation directory. Defaults to
#'   \code{Sys.getenv("FSLDIR")}; when empty and \code{download = TRUE} the
#'   Julich-Brain FSL cache is downloaded.
#' @param download Logical. Download the Julich-Brain FSL cache when
#'   \code{fsl_dir} is unset.
#'
#' @return A list with classes \code{c("visual", "volatlas", "atlas")}
#'   containing the V1-V5 regions per hemisphere.
#'
#' @references
#' Amunts, K., Mohlberg, H., Bludau, S., & Zilles, K. (2020). Julich-Brain: A
#' 3D probabilistic atlas of the human brain's cytoarchitecture. Science,
#' 369(6506), 988-992. \doi{10.1126/science.abb4588}
#'
#' @seealso \code{\link{get_wang_atlas}}, \code{\link{get_visfatlas}},
#'   \code{\link{get_julich_brain_atlas}}.
#'
#' @examples
#' \dontrun{
#' v <- get_visual_atlas()
#' v$labels
#' get_roi(v, label = "V1", hemi = "left")
#' }
#'
#' @importFrom neuroim2 read_vol ClusteredNeuroVol NeuroVol space
#' @export
get_visual_atlas <- function(outspace = NULL,
                             smooth = FALSE,
                             resolution = NULL,
                             fsl_dir = Sys.getenv("FSLDIR"),
                             download = TRUE) {
  jul <- get_julich_brain_atlas(
    fsl_dir = fsl_dir,
    download = download,
    resolution = resolution
  )

  area <- .julich_visual_area(jul$labels)
  keep <- which(!is.na(area))
  if (length(keep) == 0L) {
    cli::cli_abort(
      c(
        "No early visual regions were found in the Julich-Brain atlas.",
        "i" = "Expected labels like {.val GM Visual cortex V1 BA17 L}."
      ),
      class = c("neuroatlas_error_visual", "neuroatlas_error")
    )
  }

  vis_ids <- jul$ids[keep]
  vis_area <- area[keep]
  vis_hemi <- .julich_label_hemi(jul$labels[keep], fallback = jul$hemi[keep])

  ord <- order(vis_ids)
  vis_ids <- vis_ids[ord]
  vis_area <- vis_area[ord]
  vis_hemi <- vis_hemi[ord]

  # Subset the Julich clustered volume to the visual region ids.
  jvol <- jul$atlas
  dense <- array(0, dim = dim(jvol))
  dense[jvol@mask] <- jvol@clusters
  dense[!(dense %in% vis_ids)] <- 0
  vol <- neuroim2::NeuroVol(dense, neuroim2::space(jvol))

  template_space <- .template_space_from_outspace(outspace, default_space = "MNI152")
  if (!is.null(outspace) && !methods::is(outspace, "NeuroSpace")) {
    outspace <- .resolve_template_input(outspace, target_type = "NeuroSpace")
    if (is.null(outspace) || !methods::is(outspace, "NeuroSpace")) {
      stop("'outspace' must resolve to a valid NeuroSpace object")
    }
  }
  if (!is.null(outspace)) {
    vol <- resample(vol, outspace, smooth)
  }

  actual_ids <- sort(unique(as.integer(vol[vol != 0])))
  keep2 <- match(actual_ids, vis_ids)
  if (anyNA(keep2)) {
    stop("Visual atlas volume contains ids absent from the relabel table")
  }
  vis_area <- vis_area[keep2]
  vis_hemi <- vis_hemi[keep2]

  hemi_abbr <- ifelse(vis_hemi == "left", "L",
                      ifelse(vis_hemi == "right", "R", "NA"))
  label_map <- as.list(actual_ids)
  names(label_map) <- paste0(vis_area, "_", hemi_abbr)
  vol <- neuroim2::ClusteredNeuroVol(
    as.logical(vol),
    clusters = vol[vol != 0],
    label_map = label_map
  )

  ref <- new_atlas_ref(
    family = "visual",
    model = "JulichVisualV1toV5",
    representation = "derived",
    template_space = template_space,
    coord_space = "MNI152",
    resolution = if (is.null(resolution)) NA_character_ else resolution,
    provenance = "Derived from Julich-Brain (see get_julich_brain_atlas).",
    source = "julich_visual_subset",
    lineage = "Subset of Julich-Brain visual cortex labels relabeled V1-V5.",
    confidence = if (is.null(outspace)) "high" else "approximate",
    notes = "Julich defines only ventral V3 (V3V) and single V4/V5 per hemisphere."
  )

  artifacts <- .new_atlas_artifact(
    role = "summary_label_volume",
    family = "visual",
    model = "JulichVisualV1toV5",
    source_name = "Julich-Brain (FSL)",
    source_url = "https://www.nitrc.org/projects/fsl_atlas",
    source_ref = "Juelich.xml",
    citation_doi = "10.1126/science.abb4588",
    template_space = "MNI152",
    coord_space = "MNI152",
    lineage = "Visual subset (V1-V5) of the Julich-Brain maximum-probability map.",
    confidence = "high",
    notes = "Derived atlas; see get_julich_brain_atlas() provenance for the source."
  )

  history <- .new_atlas_history(
    action = "derive",
    representation = "derived",
    from_template_space = "MNI152",
    to_template_space = template_space,
    from_coord_space = "MNI152",
    to_coord_space = "MNI152",
    status = "available",
    confidence = if (is.null(outspace)) "high" else "approximate",
    details = "Extracted and relabeled Julich-Brain visual areas as V1-V5."
  )

  new_atlas(
    name = "Early visual cortex (V1-V5)",
    atlas = vol,
    ids = actual_ids,
    labels = vis_area,
    orig_labels = jul$labels[keep][ord][keep2],
    hemi = vis_hemi,
    cmap = .wang_visual_colors()[seq_along(actual_ids), , drop = FALSE],
    subclass = c("visual", "volatlas"),
    ref = ref,
    artifacts = artifacts,
    history = history
  )
}


#' Map Julich-Brain visual labels to canonical V1-V5 names.
#'
#' @keywords internal
#' @noRd
.julich_visual_area <- function(labels) {
  area <- rep(NA_character_, length(labels))
  area[grepl("Visual cortex V1 BA17", labels, fixed = TRUE)] <- "V1"
  area[grepl("Visual cortex V2 BA18", labels, fixed = TRUE)] <- "V2"
  area[grepl("Visual cortex V3V", labels, fixed = TRUE)] <- "V3"
  area[grepl("Visual cortex V4", labels, fixed = TRUE)] <- "V4"
  area[grepl("Visual cortex V5", labels, fixed = TRUE)] <- "V5"
  area
}


#' Derive hemisphere from a Julich label suffix (" L" / " R"), with fallback.
#'
#' @keywords internal
#' @noRd
.julich_label_hemi <- function(labels, fallback = NULL) {
  hemi <- ifelse(grepl("\\bL$", labels), "left",
                 ifelse(grepl("\\bR$", labels), "right", NA_character_))
  if (!is.null(fallback)) {
    hemi[is.na(hemi)] <- fallback[is.na(hemi)]
  }
  hemi
}
