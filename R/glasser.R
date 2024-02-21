#' Retrieve and load Glasser360 network parcellation from PennBBL github repository
#'
#' @param parcels the number rof parcels in hte atlas
#' @param networks the network number
#' @param resolution the resolution of the atlas in MNI space (1mm or 2mm)
#' @param outdim an optional 3-element vector indicating dimensions to resample atlas to.
#' @export
#'
#' @importFrom neuroim2 read_vol
#' @importFrom downloader download
#'
#' @examples
#'
#' v1 <- get_glasser_atlas()
#' 
#'
#' @return
#'
#' a list with three elemenents:
#'
#' \describe{
#'   \item{atlas}{ a NeuroVol instance with integer indices coding the ROIs}
#'   \item{cmap}{ a data.frame indicating the colors associated with each ROI}
#'   \item{legend}{a data.frame providing label information about each ROI}
#' }
#'
#' @details
#'
#' Files are downloaded from the github repository: https://github.com/PennBBL/xcpEngine/
get_glasser_atlas <- function(outspace=NULL) {
  
  fname <- "glasser360MNI.nii.gz"
  rpath= "https://github.com/PennBBL/xcpEngine/raw/master/atlas/glasser360/"
  path <- paste0(rpath,fname)
  
  des <- paste0(tempdir(), "/", fname)
  ret <- download(path, des)
  
  vol <- neuroim2::read_vol(des)
  
  if (!is.null(outspace)) {
    vol <- resample(vol, outspace)
  }
  
  label_name <- "glasser360NodeNames.txt"
  des2 <- paste0(tempdir(), "/", label_name)
  ret <- download(paste0(rpath, label_name), des2)
  
  labels <- read.table(des2, as.is=TRUE)
  cols <- t(col2rgb(rainbow(nrow(labels))))
  colnames(cols) <- c("red", "green", "blue")
  cols <- as.data.frame(cols)
  hemi <- tolower(sapply(strsplit(labels[,1], "_"), "[[", 1))
  region <- sapply(strsplit(labels[,1], "_"), "[[", 2)
  
  cids <- 1:nrow(labels)
  label_map <- as.list(cids)
  names(label_map) <- region
  
  vol <- neuroim2::ClusteredNeuroVol(as.logical(vol), clusters=vol[vol!=0], label_map=label_map)
  

  ret <- list(
    name="Glasser360",
    atlas=vol,
    cmap=cols,
    ids=1:nrow(labels),
    labels=region,
    hemi=hemi)
  
  class(ret) <- c("glasser", "atlas")
  ret
}


#' @export
#' @import ggsegGlasser 
#' @importFrom ggiraph geom_polygon_interactive
map_atlas.glasser <- function(x, vals, thresh=c(0,0), pos=FALSE, ...) {
  fun <- if (pos) {
    identity
  } else {
    abs
  }
  
  ids <- ifelse(x$hemi == "left", paste0("lh_L_", x$label), paste0("rh_R_", x$label))
  
  ret <- tibble(statistic=vals, region=x$labels, label=ids, hemi=x$hemi)
  
  
  rboth <- ggsegGlasser::glasser %>%
    as_tibble() %>%
    left_join(ret) %>%
    mutate(statistic=ifelse(fun(statistic) <= thresh[1] | fun(statistic) > thresh[2], 
                            statistic, NA)) %>%
    as_brain_atlas()
  
  
  # ggseg(atlas=rboth, position="stacked", colour="gray", interactive=FALSE, guide=TRUE,
  #       mapping=aes(fill=statistic, tooltip=region,data_id=label)) + scale_fill_distiller(palette="Spectral", 
  #                                                                                         #breaks=seq(limits[1], limits[2], length.out=10),
  #                                                                                         limits=lim,
  #                                                                                         direction=-1,
  #                                                                                         #na.value="grey",
  #                                                                                         oob=squish) 
  # 
  rboth
  
}


#' @inheritParams map_atlas
#' @export
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom ggiraph girafe opts_tooltip opts_hover opts_selection
#' @importFrom ggplot2 aes
#' @importFrom scales squish
#' @import scico
plot.glasser <- function(x, y, vals, thresh=c(0,0), pos=FALSE, position="stacked", 
                         colour="gray", guide=TRUE, palette="cork", lim=range(vals), 
                         ...) {
  gatl <- map_atlas(x,vals,thresh=thresh, pos=pos)
  
  gg <- ggseg(atlas=gatl, position=position, colour=colour, interactive=FALSE, guide=guide,
              mapping=aes(fill=statistic, tooltip=region,data_id=label)) + 
              scale_fill_scico(palette=palette, 
              #breaks=seq(limits[1], limits[2], length.out=10),
              limits=lim,
              direction=-1,
              #na.value="grey",
              oob=scales::squish)
  
  
  girafe(ggobj=gg,width_svg=8, height_svg=6, options=list(
    opts_tooltip(opacity = .7,
                 css="font-family: Arial, Helvetica, sans-serif;"),
    opts_hover(css = "fill:yellow;"),
    opts_selection(css="fill:red;", type = "single", 
                   only_shiny = FALSE)))
  
}
