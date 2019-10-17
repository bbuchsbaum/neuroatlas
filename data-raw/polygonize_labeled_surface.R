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

    lab <- stringr::str_remove(annot@labels[id], "17Networks_[RL]H_")
    names(rstobj) <- lab

    ret[[id]] <- list(
      hemi=hemi,
      side=side,
      id=id,
      region=annot@labels[id],
      rstobj=rstobj
    )

  }

  rgl.close()

  ret
}

ras_lat_lh <- do_snapshots("lh", "lateral")
ras_med_lh <- do_snapshots("lh", "medial")
ras_lat_rh <- do_snapshots("rh", "lateral")
ras_med_rh <- do_snapshots("rh", "medial")

contourobjs <- list(ras_lat_lh, ras_med_lh, ras_lat_rh, ras_med_rh)
kp <- !map_lgl(contourobjs, is.null)

contourobjsDF <- do.call(rbind, contourobjs)


schaefer.df <- filter(ho.df, kp)
ho.df <- bind_cols(contourobjsDF, ho.df)
## Now we need to place them into their own panes
## Bounding box for all
bball <- st_bbox(ho.df)
ho.df <-  mutate(ho.df, geometry=geometry - bball[c("xmin", "ymin")])

## ifelse approach doesn't seem to work, so split it up
ho.dfA <- ho.df %>%
  filter(hemi=="lh", side=="med") %>%
  mutate(geometry=geometry+c(600,0))

ho.dfB <- ho.df %>%
  filter(hemi=="rh", side=="med") %>%
  mutate(geometry=geometry+c(2*600,0))

ho.dfC <- ho.df %>%
  filter(hemi=="rh", side=="lat") %>%
  mutate(geometry=geometry+c(3*600,0))

ho.dfD <- ho.df %>%
  filter(hemi=="lh", side=="lat")

ho.df.panes <- rbind(ho.dfD, ho.dfA, ho.dfB, ho.dfC)
#ho.df.panes.simple <- st_simplify(ho.df.panes, preserveTopology = TRUE, dTolerance=0.75)
ho.df.panes.simple <- rmapshaper::ms_simplify(ho.df.panes)

plot(ho.df.panes.simple)

library(ggseg)
library(ggsegExtra)

## Not sure whether the range of values really matters. The other atlases look like they
## may be giving the coordinates in physical units of some sort.
## Lets pretend each picture is 10cm square. Divide point values by 60 at the end.

ho.df.final <- mutate(ho.df.panes.simple,
                      id=1:nrow(ho.df.panes.simple),
                      coords = map(geometry, ~(st_coordinates(.x)[, c("X", "Y")])),
                      coords = map(coords, as.tibble),
                      coords = map(coords, ~mutate(.x, order=1:nrow(.x))))
ho.df.final$geometry <- NULL
ho.df.final <- unnest(ho.df.final, .drop=TRUE)
ho.df.final <- rename(ho.df.final, long=X, lat=Y)
ggseg(atlas=ho.df.final, mapping=aes(fill=area), color="white") + theme(legend.position = "none")

save(ho.df.panes.simple, ho.df.final, file="ho_atlases.Rda")
