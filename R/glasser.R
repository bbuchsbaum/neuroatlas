#' Load Glasser Atlas
#'
#' @description
#' Retrieves and loads the Glasser360 cortical parcellation atlas from the PennBBL
#' repository. The atlas provides a detailed parcellation of the human cerebral cortex
#' based on multi-modal neuroimaging data.
#'
#' @details
#' The Glasser atlas divides each hemisphere into 180 areas (360 total) based on
#' cortical architecture, function, connectivity, and topography. The atlas is
#' downloaded from the PennBBL xcpEngine repository and includes:
#' \itemize{
#'   \item Volume data in MNI space
#'   \item Region labels and hemisphere information
#'   \item Color specifications for visualization
#' }
#'
#' @param outspace Optional \code{NeuroSpace} object specifying desired output space.
#'   If provided, the atlas will be resampled to this space. Default: NULL
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
#' Atlas files are downloaded from:
#' \url{https://github.com/PennBBL/xcpEngine/tree/master/atlas/glasser360}
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
#' @importFrom downloader download
#' @importFrom utils read.table
#' @importFrom grDevices col2rgb rainbow
#' @export
get_glasser_atlas <- function(outspace=NULL) {
  # Download and read atlas volume
  fname <- "glasser360MNI.nii.gz"
  rpath <- "https://github.com/PennBBL/xcpEngine/raw/master/atlas/glasser360/"
  path <- paste0(rpath, fname)
  
  des <- paste0(tempdir(), "/", fname)
  ret <- download(path, des)
  
  vol <- neuroim2::read_vol(des)
  
  if (!is.null(outspace)) {
    vol <- resample(vol, outspace)
  }
  
  # Download and process labels
  label_name <- "glasser360NodeNames.txt"
  des2 <- paste0(tempdir(), "/", label_name)
  ret <- download(paste0(rpath, label_name), des2)
  
  labels <- read.table(des2, as.is=TRUE)
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

  # Return atlas object
  ret <- list(
    name="Glasser360",
    atlas=vol,
    cmap=cols,
    ids=1:n,
    labels=region,
    orig_labels=orig_labels,
    hemi=hemi,
    network=NULL)

  # Build roi_metadata tibble
  ret$roi_metadata <- tibble::tibble(
    id = ret$ids,
    label = ret$labels,
    label_full = ret$orig_labels,
    hemi = ret$hemi,
    color_r = as.integer(cols$red),
    color_g = as.integer(cols$green),
    color_b = as.integer(cols$blue)
  )

  class(ret) <- c("glasser", "atlas")
  ret
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
#' @importFrom scico scale_fill_scico
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

  ret <- list(
    surf_type = surf,
    surface_space = "fsaverage",
    lh_atlas = lh,
    rh_atlas = rh,
    name = "Glasser-MMP1 fsaverage",
    cmap = cmap,
    ids = ids,
    labels = labels,
    orig_labels = orig_labels,
    hemi = hemi
  )

  # Build roi_metadata tibble
  ret$roi_metadata <- tibble::tibble(
    id = ret$ids,
    label = ret$labels,
    label_full = ret$orig_labels,
    hemi = ret$hemi,
    color_r = as.integer(cmap$red),
    color_g = as.integer(cmap$green),
    color_b = as.integer(cmap$blue)
  )

  class(ret) <- c("glasser_surf", "surfatlas", "atlas")
  ret
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
    "https://figshare.com/ndownloader/files/5528816"
  } else {
    "https://figshare.com/ndownloader/files/5528819"
  }

  tmp <- tempfile(fileext = ".annot")
  downloader::download(url, tmp)

  ok <- file.copy(tmp, fpath, overwrite = TRUE)
  if (!ok) {
    stop("Failed to cache Glasser annotation file at ", fpath)
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

  geom <- if (!is.null(surf_path) && file.exists(surf_path)) {
    neurosurf::read_surf_geometry(surf_path)
  } else {
    # Fallback to packaged fsaverage surfaces if TemplateFlow is unavailable
    data("fsaverage", package = "neuroatlas", envir = environment())
    fsavg_obj <- get("fsaverage", envir = environment())
    surf_name <- paste0(hemi, "_", surf)
    if (is.null(fsavg_obj[[surf_name]])) {
      stop("Failed to load fsaverage surface geometry for hemisphere ", hemi, " (", surf, ")")
    }
    fsavg_obj[[surf_name]]
  }

  annot <- suppressWarnings(
    neurosurf::read_freesurfer_annot(annot_path, geom)
  )

  # Basic sanity check: number of vertices should match
  n_vertices <- ncol(geom@mesh$vb) - 1L
  if (length(annot@data) != n_vertices) {
    stop("Vertex mismatch between Glasser annotation and fsaverage surface for hemisphere ", hemi)
  }

  annot
}
