#' Load Glasser Atlas
#'
#' @description
#' Retrieves and loads a volumetric representation of the Glasser/HCP-MMP 1.0
#' cortical parcellation atlas.
#'
#' @details
#' The Glasser atlas divides each hemisphere into 180 areas (360 total) based on
#' cortical architecture, function, connectivity, and topography.
#'
#' Supported sources:
#' \itemize{
#'   \item \code{"mni2009c"} (default): MNI152NLin2009cAsym-provenance volume
#'     file (\code{MMP_in_MNI_corr.nii.gz}).
#'   \item \code{"xcpengine"}: legacy xcpEngine Glasser360 volume
#'     (\code{glasser360MNI.nii.gz}) with less explicit template provenance.
#' }
#'
#' If \code{source = "mni2009c"} is unavailable at runtime, the loader
#' automatically falls back to \code{"xcpengine"} and marks confidence as
#' \code{"uncertain"}.
#'
#' Region labels are read from the xcpEngine node-name table to provide stable
#' parcel naming across sources.
#'
#' @param outspace Optional \code{NeuroSpace} object specifying desired output space.
#'   If provided, the atlas will be resampled to this space. Default: NULL
#' @param source Volume source to use. One of \code{"mni2009c"} (default) or
#'   \code{"xcpengine"}.
#'
#' @return A list with class 'glasser' and 'atlas' containing:
#' \describe{
#'   \item{name}{Character string "Glasser360"}
#'   \item{atlas}{A \code{ClusteredNeuroVol} object containing the parcellation}
#'   \item{cmap}{Data frame with RGB color specifications for each region}
#'   \item{ids}{Integer vector of region IDs (1:360)}
#'   \item{labels}{Character vector of anatomical labels}
#'   \item{hemi}{Character vector indicating hemisphere ('left' or 'right')}
#' }
#'
#' @references
#' Glasser, M. F., et al. (2016). A multi-modal parcellation of human cerebral
#' cortex. Nature, 536(7615), 171-178.
#'
#' @source
#' Source-specific links are recorded in \code{atlas_ref(atlas)$provenance}.
#'
#' @examples
#' \dontrun{
#' # Load atlas in native space
#' atlas <- get_glasser_atlas()
#'
#' # View region labels
#' head(atlas$labels)
#'
#' # Check number of regions per hemisphere
#' table(atlas$hemi)
#' }
#'
#' @importFrom neuroim2 read_vol ClusteredNeuroVol
#' @importFrom utils read.table
#' @importFrom grDevices col2rgb rainbow
#' @export
get_glasser_atlas <- function(outspace=NULL,
                              source = c("mni2009c", "xcpengine")) {
  source <- match.arg(source)
  requested_source <- source
  source_info <- .glasser_volume_source_info(source)

  status <- .neuroatlas_try_download(
    url = source_info$volume_url,
    dest = file.path(tempdir(), source_info$fname),
    description = paste0("Glasser volume (source='", source, "')")
  )

  if (!status$ok && identical(source, "mni2009c")) {
    cli::cli_warn(
      c(
        "Default Glasser source {.val mni2009c} was unavailable; falling back to {.val xcpengine}.",
        "i" = "Original error: {conditionMessage(status$error)}",
        "!" = "Provenance confidence is downgraded to {.val uncertain}."
      ),
      class = c("neuroatlas_warn_fallback", "neuroatlas_warn")
    )
    source <- "xcpengine"
    source_info <- .glasser_volume_source_info(source)
    # Hard download: if fallback also fails, surface the error.
    vol_path <- .neuroatlas_download(
      url = source_info$volume_url,
      dest = file.path(tempdir(), source_info$fname),
      description = "Glasser volume (xcpengine fallback)"
    )
  } else if (!status$ok) {
    # Caller explicitly asked for xcpengine and it failed — surface the error.
    cli::cli_abort(
      c(
        "Failed to download Glasser volume (source={.val {source}}).",
        "i" = "URL: {.url {source_info$volume_url}}",
        "x" = "{conditionMessage(status$error)}"
      ),
      class = c("neuroatlas_error_download", "neuroatlas_error"),
      parent = status$error
    )
  } else {
    vol_path <- status$path
  }

  template_space <- .template_space_from_outspace(
    outspace,
    default_space = if (source == "mni2009c") {
      "MNI152NLin2009cAsym"
    } else {
      "MNI152_unspecified"
    }
  )

  vol <- neuroim2::read_vol(vol_path)
  
  if (!is.null(outspace)) {
    vol <- resample(vol, outspace)
  }
  
  # Download and process labels
  label_name <- "glasser360NodeNames.txt"
  label_path <- .neuroatlas_download(
    url = source_info$label_url,
    dest = file.path(tempdir(), label_name),
    min_size = 16L,
    description = "Glasser label table"
  )

  labels <- read.table(label_path, as.is = TRUE)
  cols <- t(col2rgb(rainbow(nrow(labels))))
  colnames(cols) <- c("red", "green", "blue")
  cols <- as.data.frame(cols)
  hemi <- tolower(sapply(strsplit(labels[,1], "_"), "[[", 1))
  region <- sapply(strsplit(labels[,1], "_"), "[[", 2)
  orig_labels <- labels[,1]
  
  # Create label mapping
  cids <- 1:nrow(labels)
  label_map <- as.list(cids)
  names(label_map) <- region
  
  vol <- neuroim2::ClusteredNeuroVol(as.logical(vol),
                                    clusters=vol[vol!=0],
                                    label_map=label_map)

  n <- nrow(labels)

  ref <- new_atlas_ref(
    family = "glasser",
    model = "HCP-MMP1.0",
    representation = "volume",
    template_space = template_space,
    coord_space = "MNI152",
    resolution = if (source == "mni2009c") "1mm" else NA_character_,
    provenance = source_info$provenance,
    source = source,
    lineage = if (!identical(source, requested_source)) {
      paste(source_info$lineage, "(fallback from mni2009c request)")
    } else {
      source_info$lineage
    },
    confidence = if (source == "mni2009c") {
      if (is.null(outspace)) "high" else "approximate"
    } else {
      "uncertain"
    },
    notes = source_info$notes
  )

  native_space <- if (source == "mni2009c") {
    "MNI152NLin2009cAsym"
  } else {
    "MNI152_unspecified"
  }

  artifacts <- dplyr::bind_rows(
    .new_atlas_artifact(
      role = "parcellation_volume",
      family = "glasser",
      model = "HCP-MMP1.0",
      variant = source,
      source_name = if (source == "mni2009c") {
        "Raj-Lab-UCSF mirror"
      } else {
        "xcpEngine"
      },
      source_url = source_info$volume_url,
      source_ref = source_info$fname,
      citation_doi = "10.1038/nature18933",
      file_name = source_info$fname,
      template_space = native_space,
      coord_space = "MNI152",
      resolution = if (source == "mni2009c") "1mm" else NA_character_,
      lineage = source_info$lineage,
      confidence = if (source == "mni2009c") "high" else "uncertain",
      notes = source_info$notes
    ),
    .new_atlas_artifact(
      role = "label_table",
      family = "glasser",
      model = "HCP-MMP1.0",
      variant = "xcpengine_labels",
      source_name = "xcpEngine",
      source_url = source_info$label_url,
      source_ref = "glasser360NodeNames.txt",
      citation_doi = "10.1038/nature18933",
      file_name = "glasser360NodeNames.txt",
      lineage = "Parcel label table used for stable Glasser naming.",
      confidence = "high"
    )
  )

  history <- .new_atlas_history(
    action = "load",
    representation = "volume",
    from_template_space = native_space,
    to_template_space = native_space,
    from_coord_space = "MNI152",
    to_coord_space = "MNI152",
    status = "available",
    confidence = if (source == "mni2009c") "high" else "uncertain",
    details = paste0("Loaded Glasser volume from source='", source, "'.")
  )
  if (!is.null(outspace)) {
    history <- dplyr::bind_rows(
      history,
      .new_atlas_history(
        action = "resample",
        representation = "volume",
        from_template_space = native_space,
        to_template_space = template_space,
        from_coord_space = "MNI152",
        to_coord_space = "MNI152",
        status = "available",
        confidence = "approximate",
        details = "Resampled atlas to requested output space."
      )
    )
  }

  new_atlas(
    name = "Glasser360",
    atlas = vol,
    ids = seq_len(n),
    labels = region,
    orig_labels = orig_labels,
    hemi = hemi,
    cmap = cols,
    subclass = "glasser",
    ref = ref,
    artifacts = artifacts,
    history = history
  )
}

#' @rdname map_atlas
#' @importFrom dplyr left_join mutate
#' @importFrom tibble tibble
#' @export
map_atlas.glasser <- function(x, vals, thresh = NULL, pos = FALSE, ...) {
  # Delegate to the base atlas method (no ggseg dependency)
  map_atlas.atlas(x, vals, thresh = thresh, pos = pos, ...)
}

#' Plot Glasser Atlas
#'
#' @description
#' Visualise a Glasser atlas object. For surface atlases (\code{glasser_surf}),
#' renders via \code{\link{plot_brain}()}. For volumetric Glasser atlases,
#' falls back to the base \code{\link{plot.atlas}()} method.
#'
#' @param x A Glasser atlas object
#' @param y Ignored (required for compatibility with generic plot method)
#' @param vals Numeric vector of values to visualize. If NULL (default), all regions
#'   will be assigned a value of 1, creating a uniform visualization
#' @param thresh Numeric vector of length 2 for thresholding values
#' @param pos Logical. If TRUE, uses raw values for thresholding
#' @rdname plot-methods
#' @param palette Character. Name of scico color palette
#' @param lim Numeric vector of length 2 for color scale limits. If NULL, will be
#'   set to range of vals
#' @param ... Additional arguments passed to methods
#' @return A ggplot2 or ggiraph object
#'
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate left_join
#' @importFrom ggiraph girafe opts_tooltip opts_hover opts_selection
#' @importFrom ggplot2 aes
#' @export
plot.glasser <- function(x, y, vals = NULL, thresh = c(0, 0), pos = FALSE,
                         palette = "cork", lim = NULL, ...) {
  if (inherits(x, "surfatlas")) {
    plot_brain(x, vals = vals, palette = palette, lim = lim, ...)
  } else {
    # Volumetric glasser — use base atlas plot method
    plot.atlas(x, y, ...)
  }
}

#' @rdname print-methods
#' @importFrom crayon bold green blue red white yellow
#' @importFrom cli rule symbol
#' @export
print.glasser <- function(x, ...) {
  # Header with fancy border
  cat(cli::rule(left = crayon::bold(crayon::blue("Glasser Atlas Summary")), 
                col = "cyan", width = 65), "\n\n")
  
  # Basic info section
  cat(crayon::yellow(cli::symbol$info), " ", 
      crayon::bold("Atlas Type: "), 
      crayon::white("Glasser Multi-Modal Parcellation"), "\n", sep="")
  
  cat(crayon::yellow(cli::symbol$info), " ",
      crayon::bold("Resolution: "), 
      crayon::white("MNI Space"), "\n", sep="")
  
  # Volume dimensions
  dims <- dim(x$atlas)
  cat(crayon::yellow(cli::symbol$info), " ",
      crayon::bold("Dimensions: "), 
      crayon::white(paste0(dims[1], " x ", dims[2], " x ", dims[3])), "\n\n", sep="")
  
  # Region counts
  total_regions <- length(x$ids)
  left_regions <- sum(x$hemi == "left")
  right_regions <- sum(x$hemi == "right")
  
  cat(crayon::green(cli::symbol$circle_filled), " ",
      crayon::bold("Region Summary:"), "\n", sep="")
  
  cat(crayon::blue("|-"), " Total Regions:      ", 
      crayon::white(total_regions), "\n", sep="")
  cat(crayon::blue("|-"), " Left Hemisphere:    ", 
      crayon::white(left_regions), "\n", sep="")
  cat(crayon::blue("\\-"), " Right Hemisphere:   ", 
      crayon::white(right_regions), "\n\n", sep="")
  
  # Sample regions
  cat(crayon::green(cli::symbol$circle_filled), " ",
      crayon::bold("Example Regions:"), "\n", sep="")
  
  # Show first 3 regions from each hemisphere
  left_examples <- head(x$labels[x$hemi == "left"], 3)
  right_examples <- head(x$labels[x$hemi == "right"], 3)
  
  cat(crayon::blue("|-"), " Left:  ", 
      crayon::white(paste(left_examples, collapse=", ")), "...\n", sep="")
  cat(crayon::blue("\\-"), " Right: ", 
      crayon::white(paste(right_examples, collapse=", ")), "...\n", sep="")
  
  # Footer
  cat("\n", cli::rule(
    left = crayon::blue(cli::symbol$info), 
    right = "Use plot() for visualization",
    col = "cyan", width = 65), "\n", sep="")
}


# Glasser surface atlas --------------------------------------------------------

#' Glasser Surface Atlas (fsaverage)
#'
#' @description
#' Load the Glasser HCP-MMP1.0 cortical parcellation projected to the
#' FreeSurfer \code{fsaverage} surface, as distributed by Kathryn Mills
#' (see Figshare dataset "HCP-MMP1.0 projected on fsaverage").
#' The result is a pair of neurosurf \code{LabeledNeuroSurface} objects plus
#' atlas metadata.
#'
#' @details
#' This function uses:
#' \itemize{
#'   \item fsaverage surface geometry from TemplateFlow via
#'         \code{\link{get_surface_template}}
#'   \item fsaverage \code{.annot} files from the Mills Figshare distribution
#'         (\code{lh.HCP-MMP1.annot}, \code{rh.HCP-MMP1.annot})
#' }
#' Currently only the \code{"fsaverage"} surface space is supported.
#'
#' @param space Surface space / mesh template. Only \code{"fsaverage"} is
#'   supported at present.
#' @param surf Surface type. One of \code{"pial"}, \code{"white"},
#'   \code{"inflated"}, or \code{"midthickness"}.
#' @param use_cache Logical. Whether to cache downloaded annotation files in
#'   the neuroatlas cache directory. Default: \code{TRUE}.
#'
#' @return A list with classes \code{c("glasser_surf","surfatlas","atlas")}
#'   containing:
#'   \itemize{
#'     \item \code{lh_atlas}, \code{rh_atlas}: \code{LabeledNeuroSurface}
#'       objects for left and right hemispheres.
#'     \item \code{surf_type}: requested surface type.
#'     \item \code{surface_space}: surface template space ("fsaverage").
#'     \item \code{ids}, \code{labels}, \code{orig_labels},
#'       \code{hemi}, \code{cmap}: atlas metadata.
#'   }
#'
#' @examples
#' \dontrun{
#' # Glasser MMP1.0 on fsaverage pial surface
#' atl <- glasser_surf(space = "fsaverage", surf = "pial")
#' }
#'
#' @export
glasser_surf <- function(space = "fsaverage",
                         surf = c("pial", "white", "inflated", "midthickness"),
                         use_cache = TRUE) {
  space <- match.arg(space, c("fsaverage", "fsaverage5", "fsaverage6"))
  surf <- match.arg(surf, c("pial", "white", "inflated", "midthickness"))

  if (!identical(space, "fsaverage")) {
    stop("Glasser surface atlas is currently only available in 'fsaverage' space.")
  }

  lh <- .glasser_fsaverage_surface_hemi("lh", surf, use_cache = use_cache)
  rh <- .glasser_fsaverage_surface_hemi("rh", surf, use_cache = use_cache)

  # Extract label information from hemispheres
  lh_labels <- lh@labels
  rh_labels <- rh@labels

  labels <- c(lh_labels, rh_labels)
  ids <- seq_along(labels)
  hemi <- c(rep("left", length(lh_labels)), rep("right", length(rh_labels)))
  orig_labels <- labels

  # Build RGB colormap from hex colours
  lh_cols <- lh@cols
  rh_cols <- rh@cols
  all_cols <- c(lh_cols, rh_cols)

  rgb_mat <- t(grDevices::col2rgb(all_cols))
  colnames(rgb_mat) <- c("red", "green", "blue")
  cmap <- as.data.frame(rgb_mat)

  ref <- new_atlas_ref(
    family = "glasser",
    model = "HCP-MMP1.0",
    representation = "surface",
    template_space = "fsaverage",
    coord_space = get_surface_coordinate_space("fsaverage"),
    density = "164k",
    provenance = "https://doi.org/10.6084/m9.figshare.3498446",
    source = "mills_figshare_fsaverage",
    lineage = "Projected from HCP fsLR32k labels to fsaverage surface.",
    confidence = "high"
  )

  artifacts <- dplyr::bind_rows(
    .new_atlas_artifact(
      role = "annotation_lh",
      family = "glasser",
      model = "HCP-MMP1.0",
      source_name = "Figshare",
      source_url = "https://doi.org/10.6084/m9.figshare.3498446",
      source_ref = "lh.HCP-MMP1.annot",
      citation_doi = "10.6084/m9.figshare.3498446",
      template_space = "fsaverage",
      coord_space = get_surface_coordinate_space("fsaverage"),
      density = "164k",
      hemi = "left",
      lineage = "Projected Glasser annotation distributed on fsaverage.",
      confidence = "high"
    ),
    .new_atlas_artifact(
      role = "annotation_rh",
      family = "glasser",
      model = "HCP-MMP1.0",
      source_name = "Figshare",
      source_url = "https://doi.org/10.6084/m9.figshare.3498446",
      source_ref = "rh.HCP-MMP1.annot",
      citation_doi = "10.6084/m9.figshare.3498446",
      template_space = "fsaverage",
      coord_space = get_surface_coordinate_space("fsaverage"),
      density = "164k",
      hemi = "right",
      lineage = "Projected Glasser annotation distributed on fsaverage.",
      confidence = "high"
    ),
    .new_atlas_artifact(
      role = "geometry_lh",
      family = "glasser",
      model = "HCP-MMP1.0",
      source_name = "TemplateFlow",
      source_url = "https://www.templateflow.org",
      template_space = "fsaverage",
      coord_space = get_surface_coordinate_space("fsaverage"),
      density = "164k",
      hemi = "left",
      lineage = paste0("TemplateFlow surface geometry (", surf, ")."),
      confidence = "high"
    ),
    .new_atlas_artifact(
      role = "geometry_rh",
      family = "glasser",
      model = "HCP-MMP1.0",
      source_name = "TemplateFlow",
      source_url = "https://www.templateflow.org",
      template_space = "fsaverage",
      coord_space = get_surface_coordinate_space("fsaverage"),
      density = "164k",
      hemi = "right",
      lineage = paste0("TemplateFlow surface geometry (", surf, ")."),
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
    details = paste0("Loaded Glasser fsaverage surface atlas (", surf, ").")
  )

  new_surfatlas(
    name = "Glasser-MMP1 fsaverage",
    lh_atlas = lh,
    rh_atlas = rh,
    ids = ids,
    labels = labels,
    orig_labels = orig_labels,
    hemi = hemi,
    cmap = cmap,
    surf_type = surf,
    surface_space = "fsaverage",
    subclass = "glasser_surf",
    ref = ref,
    artifacts = artifacts,
    history = history
  )
}


#' @keywords internal
#' @noRd
.glasser_volume_source_info <- function(source) {
  if (identical(source, "mni2009c")) {
    return(list(
      fname = "MMP_in_MNI_corr.nii.gz",
      volume_url = paste0(
        "https://raw.githubusercontent.com/Raj-Lab-UCSF/",
        "Human_Brain_Atlases-glasser/master/MMP_in_MNI_corr.nii.gz"
      ),
      label_url = paste0(
        "https://github.com/PennBBL/xcpEngine/raw/master/atlas/",
        "glasser360/glasser360NodeNames.txt"
      ),
      provenance = paste(
        "MNI2009c volume (MMP_in_MNI_corr) mirrored at",
        "https://github.com/Raj-Lab-UCSF/Human_Brain_Atlases-glasser"
      ),
      lineage = "Surface reconstruction/projection to MNI152NLin2009cAsym volume.",
      notes = "Default source with explicit 2009c provenance."
    ))
  }

  warning(
    "Using legacy source='xcpengine' with uncertain template provenance. ",
    "Prefer source='mni2009c' for explicit 2009c alignment.",
    call. = FALSE
  )

  list(
    fname = "glasser360MNI.nii.gz",
    volume_url = paste0(
      "https://github.com/PennBBL/xcpEngine/raw/master/atlas/",
      "glasser360/glasser360MNI.nii.gz"
    ),
    label_url = paste0(
      "https://github.com/PennBBL/xcpEngine/raw/master/atlas/",
      "glasser360/glasser360NodeNames.txt"
    ),
    provenance = "https://github.com/PennBBL/xcpEngine/tree/master/atlas/glasser360",
    lineage = "xcpEngine-distributed volumetric derivative.",
    notes = "Template identity is not explicitly documented by the source."
  )
}


# Internal: Glasser Figshare annotation paths ----------------------------------

#' @keywords internal
.glasser_figshare_annot_path <- function(hemi, use_cache = TRUE) {
  hemi <- match.arg(hemi, c("lh", "rh"))

  cache_dir <- .neuroatlas_cache_dir("glasser")
  fname <- if (hemi == "lh") "lh.HCP-MMP1.annot" else "rh.HCP-MMP1.annot"
  fpath <- file.path(cache_dir, fname)

  if (use_cache && file.exists(fpath)) {
    return(fpath)
  }

  url <- if (hemi == "lh") {
    "https://ndownloader.figshare.com/files/5528816"
  } else {
    "https://ndownloader.figshare.com/files/5528819"
  }

  tmp <- tempfile(fileext = ".annot")
  .neuroatlas_download(
    url = url,
    dest = tmp,
    description = paste0("Glasser ", hemi, ".HCP-MMP1.annot")
  )

  ok <- file.copy(tmp, fpath, overwrite = TRUE)
  if (!ok) {
    cli::cli_abort(
      "Failed to cache Glasser annotation file at {.path {fpath}}.",
      class = c("neuroatlas_error_cache", "neuroatlas_error")
    )
  }

  fpath
}


# Internal: Glasser fsaverage surface loader -----------------------------------

#' @keywords internal
.glasser_fsaverage_surface_hemi <- function(hemi,
                                            surf = c("pial", "white", "inflated", "midthickness"),
                                            use_cache = TRUE) {
  hemi <- match.arg(hemi, c("lh", "rh"))
  surf <- match.arg(surf, c("pial", "white", "inflated", "midthickness"))

  annot_path <- .glasser_figshare_annot_path(hemi, use_cache = use_cache)

  hemi_tf <- if (hemi == "lh") "L" else "R"

  surf_path <- tryCatch({
    get_surface_template(
      template_id = "fsaverage",
      surface_type = surf,
      hemi = hemi_tf,
      density = "164k",
      load_as_path = TRUE
    )
  }, error = function(e) {
    NULL
  })

  geom <- NULL
  if (!is.null(surf_path) && file.exists(surf_path)) {
    geom <- tryCatch(
      neurosurf::read_surf_geometry(surf_path),
      error = function(e) NULL
    )
    # Fallback: read GIFTI directly if neurosurf parser fails
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
    # Fallback to packaged fsaverage surfaces (low-res, may cause vertex
    # mismatch with full-resolution annotation files)
    data("fsaverage", package = "neuroatlas", envir = environment())
    fsavg_obj <- get("fsaverage", envir = environment())
    surf_name <- paste0(hemi, "_", surf)
    if (is.null(fsavg_obj[[surf_name]])) {
      stop("Failed to load fsaverage surface geometry for hemisphere ", hemi, " (", surf, ")")
    }
    geom <- fsavg_obj[[surf_name]]
  }

  annot <- suppressWarnings(
    neurosurf::read_freesurfer_annot(annot_path, geom)
  )

  # Basic sanity check: number of vertices should match
  n_vertices <- ncol(geom@mesh$vb)
  if (length(annot@data) != n_vertices) {
    stop("Vertex mismatch between Glasser annotation (", length(annot@data),
         " vertices) and fsaverage surface (", n_vertices,
         " vertices) for hemisphere ", hemi)
  }

  annot
}
