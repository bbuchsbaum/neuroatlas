library(magick)

geometry = read_surf("raw-dat/fsaverage6/lh.white")
annot=read_freesurfer_annot("raw-dat/lh.Schaefer2018_1000Parcels_17Networks_order.annot", geometry)


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
