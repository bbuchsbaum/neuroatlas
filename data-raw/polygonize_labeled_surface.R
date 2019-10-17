library(magick)
library(tidyverse)
library(raster)
library(stars)
library(sf)
library(rmapshaper)
library(neurosurf)


geometry_lh = neurosurf::read_surf("fsaverage6/lh.inflated")
geometry_rh = neurosurf::read_surf("fsaverage6/rh.inflated")
annot_lh=neurosurf::read_freesurfer_annot("Schaefer-yeo/lh.Schaefer2018_400Parcels_17Networks_order.annot", geometry_lh)
annot_rh=neurosurf::read_freesurfer_annot("Schaefer-yeo/rh.Schaefer2018_400Parcels_17Networks_order.annot", geometry_rh)

origlabels <- c(annot_lh@labels, annot_rh@labels)
region <- stringr::str_remove(origlabels, "17Networks_[RL]H_")

hemi <- stringr::str_extract(origlabels, "[RL]H")
side <- stringr::str_extract(region, "...$")
region <- stringr::str_remove(region, "_...$")
origlabel <- stringr::str_remove(origlabel, "_...$")
ho.df <- tibble(area=region, hemi=hemi, side=side, label=origlabel)
ho.df <- mutate(ho.df, area=stringr::str_replace_all(area, "\\.+", " "))


mkContours <- function(rstobj){
  mx <- cellStats(rstobj, stat=max)
  # Filter out the blank images
  if (mx < 200) {
    return(NULL)
  }
  tmp.rst <- rstobj
  tmp.rst[tmp.rst == 0] <- NA

  ## levels = 50 is to remove the occasional edge point that has
  ## non zero hue.
  #cntr <- raster::rasterToPolygons(rstobj, fun = function(X)X>100, dissolve=TRUE)
  g <- st_as_sf(st_as_stars(tmp.rst), merge=TRUE, connect8=TRUE)
  ## Is it a multipolygon? Keep the biggest bit
  ## Small parts are usually corner connected single voxels
  if (nrow(g)>1) {
    gpa <- st_area(g)
    biggest <- which.max(gpa)
    g <- g[biggest,]
  }
  g <-st_sf(g)
  names(g)[[1]] <- "region"
  g$region <- names(rstobj)
  return(g)

}

do_snapshots <- function(hemi, side) {
  if (hemi == "lh") {
    geom  <- neurosurf::read_surf("fsaverage6/lh.inflated")
    annot <- neurosurf::read_freesurfer_annot("Schaefer-yeo/lh.Schaefer2018_400Parcels_17Networks_order.annot", geom)
  } else {
    geom  <- neurosurf::read_surf("fsaverage6/rh.inflated")
    annot <- neurosurf::read_freesurfer_annot("Schaefer-yeo/rh.Schaefer2018_400Parcels_17Networks_order.annot", geom)
  }
  ids <- sort(unique(annot@data))
  ret <- vector(length(ids), mode="list")
  for (id in ids) {
    tmp = annot == id
    plot(tmp, irange=c(0,1), threshold=c(-55,.3), bgcol="seashell4",cmap= rainbow(1), view=side)
    fname1 <- tempfile(fileext=".png")
    rgl.snapshot(filename=fname1)
    rgl.clear()
    im=image_read(fname1)
    ## converting and thresholding
    im <- im %>% image_convert(colorspace="HSL") %>% image_channel("1") %>% image_threshold(threshold="10%")

    tiff <- tempfile()
    image_write(im, path = tiff, format = 'tiff')
    rstobj <- raster::brick(tiff)

    ret[[id]] <- list(
      hemi=hemi,
      side=side,
      id=id,
      region=annot@labels[id],
      rstobj=rstobj
    )

  }

  ret
}

ras_lh <- do_hemi("lh")


## create an image of a single ROI
tmp = annot == 321
plot(tmp, irange=c(0,1), threshold=c(-55,.3), bgcol="seashell4",cmap= raibow(1))
rgl.snapshot("roi_321.png")
im1=image_read("roi_321.png")

## converting and thresholding
im1 %>% image_convert(colorspace="HSL") %>% image_channel("1") %>% image_threshold(threshold="10%")

## smoothing polygon
library(smoothr)
smooth(g, method="ksmooth", smoothness=2)

## converting to tiff and re-reading
tiff_file <- tempfile()
image_write(im4, path = tiff_file, format = 'tiff')
rstobj <- raster::brick(tiff_file)
