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
  
  # Return atlas object
  ret <- list(
    name="Glasser360",
    atlas=vol,
    cmap=cols,
    ids=1:nrow(labels),
    labels=region,
    orig_labels=orig_labels,
    hemi=hemi,
    network=NULL)
  
  class(ret) <- c("glasser", "atlas")
  ret
}

#' Map Values to Glasser Atlas
#'
#' @description
#' Maps numeric values to regions in the Glasser atlas for visualization using
#' ggseg plotting functions.
#'
#' @param x A Glasser atlas object
#' @param vals Numeric vector of values to map to atlas regions
#' @param thresh Numeric vector of length 2 specifying (min, max) thresholds.
#'   Values outside this range will be set to NA
#' @param pos Logical. If TRUE, uses raw values; if FALSE, uses absolute values
#'   for thresholding
#' @param ... Additional arguments passed to methods
#'
#' @return A ggseg brain atlas object with mapped values
#'
#' @import ggsegGlasser
#' @importFrom ggiraph geom_polygon_interactive
#' @importFrom dplyr left_join mutate
#' @importFrom tibble tibble
#' @export
map_atlas.glasser <- function(x, vals, thresh=c(0,0), pos=FALSE, ...) {
  fun <- if (pos) identity else abs
  
  ids <- ifelse(x$hemi == "left", 
                paste0("lh_L_", x$label), 
                paste0("rh_R_", x$label))
  
  ret <- tibble(statistic=vals, region=x$labels, label=ids, hemi=x$hemi)
  
  rboth <- ggsegGlasser::glasser %>%
    as_tibble() %>%
    left_join(ret) %>%
    mutate(statistic=ifelse(fun(statistic) <= thresh[1] | 
                           fun(statistic) > thresh[2], 
                           statistic, NA)) %>%
    as_brain_atlas()
  
  rboth
}

#' Plot Glasser Atlas
#'
#' @description
#' Creates an interactive visualization of the Glasser atlas with mapped values
#' using ggseg and ggiraph.
#'
#' @param x A Glasser atlas object
#' @param y Ignored (required for compatibility with generic plot method)
#' @param vals Numeric vector of values to visualize. If NULL (default), all regions
#'   will be assigned a value of 1, creating a uniform visualization
#' @param thresh Numeric vector of length 2 for thresholding values
#' @param pos Logical. If TRUE, uses raw values for thresholding
#' @param position Character. Layout type ("stacked" or "dispersed")
#' @param colour Character. Border color for regions
#' @param guide Logical. Whether to show color guide
#' @param palette Character. Name of scico color palette
#' @param lim Numeric vector of length 2 for color scale limits. If NULL, will be
#'   set to range of vals
#' @param ... Additional arguments passed to methods
#'
#' @return A ggiraph interactive plot object
#'
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate left_join
#' @importFrom ggiraph girafe opts_tooltip opts_hover opts_selection
#' @importFrom ggplot2 aes
#' @importFrom scico scale_fill_scico
#' @import scico
#' @export
plot.glasser <- function(x, y, vals=NULL, thresh=c(0,0), pos=FALSE, 
                        position="stacked", colour="gray", guide=TRUE,
                        palette="cork", lim=NULL, ...) {
  if (is.null(vals)) {
    vals <- rep(1, length(x$labels))
  }
  
  if (is.null(lim)) {
    lim <- range(vals)
  }
  
  gatl <- map_atlas(x, vals, thresh=thresh, pos=pos)
  
  gg <- ggseg(atlas=gatl, 
              position=position, 
              colour=colour, 
              interactive=FALSE, 
              guide=guide,
              mapping=aes(fill=statistic, 
                         tooltip=region,
                         data_id=label)) + 
    scale_fill_scico(palette=palette,
                     limits=lim,
                     direction=-1,
                     oob=scales::squish)
  
  girafe(ggobj=gg,
         width_svg=8, 
         height_svg=6, 
         options=list(
           opts_tooltip(opacity=.7,
                       css="font-family: Arial, Helvetica, sans-serif;"),
           opts_hover(css="fill:yellow;"),
           opts_selection(css="fill:red;", 
                         type="single", 
                         only_shiny=FALSE)))
}

#' Print Method for Glasser Atlas Objects
#'
#' @description
#' Displays a formatted summary of a Glasser atlas object, including region counts,
#' hemisphere distribution, and basic metadata.
#'
#' @param x A Glasser atlas object
#' @param ... Additional arguments passed to print methods
#'
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
      crayon::white(paste0(dims[1], " × ", dims[2], " × ", dims[3])), "\n\n", sep="")
  
  # Region counts
  total_regions <- length(x$ids)
  left_regions <- sum(x$hemi == "left")
  right_regions <- sum(x$hemi == "right")
  
  cat(crayon::green(cli::symbol$circle_filled), " ",
      crayon::bold("Region Summary:"), "\n", sep="")
  
  cat(crayon::blue("├─"), " Total Regions:      ", 
      crayon::white(total_regions), "\n", sep="")
  cat(crayon::blue("├─"), " Left Hemisphere:    ", 
      crayon::white(left_regions), "\n", sep="")
  cat(crayon::blue("└─"), " Right Hemisphere:   ", 
      crayon::white(right_regions), "\n\n", sep="")
  
  # Sample regions
  cat(crayon::green(cli::symbol$circle_filled), " ",
      crayon::bold("Example Regions:"), "\n", sep="")
  
  # Show first 3 regions from each hemisphere
  left_examples <- head(x$labels[x$hemi == "left"], 3)
  right_examples <- head(x$labels[x$hemi == "right"], 3)
  
  cat(crayon::blue("├─"), " Left:  ", 
      crayon::white(paste(left_examples, collapse=", ")), "...\n", sep="")
  cat(crayon::blue("└─"), " Right: ", 
      crayon::white(paste(right_examples, collapse=", ")), "...\n", sep="")
  
  # Footer
  cat("\n", cli::rule(
    left = crayon::blue(cli::symbol$info), 
    right = "Use plot() for visualization",
    col = "cyan", width = 65), "\n", sep="")
}
