olsen_dir <- "data-raw/Olsen-mtl"
library(neuroim2)

vol <- read_vol(file.path(olsen_dir, "AVG_MNI_MTL_P33.nii"))

full_label <- scan(file.path(olsen_dir, "labels.txt"), "")
hemi <- substr(full_label, 1, 1)
hemi <- ifelse(hemi == "L", "left", "right")
label <- vapply(full_label, function(x) substr(x, 3, nchar(x)), character(1))
names(label) <- NULL

#names(labels) <- c("ROINUM", "label", "red", "green", "blue")
paste(as.vector(col2rgb(x)), collapse = " ")
cmap <- rbind(t(col2rgb(cols)), t(col2rgb(cols)))

olsen_mtl <- list(
  name = "Olsen_MTL",
  atlas = vol,
  cmap = cmap,
  ids = seq_along(full_label),
  labels = label,
  orig_labels = full_label,
  hemi = hemi)

class(olsen_mtl) <- c("olsen_mtl", "atlas")
usethis::use_data(olsen_mtl, internal = FALSE, overwrite = TRUE, compress = "xz")


