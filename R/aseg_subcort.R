#' @export
get_aseg_atlas <- function(outspace=NULL) {
  fname <- system.file("extdata/atlas_aparc_aseg_prob33.nii.gz", package="neuroatlas")
  atlas <- neuroim2::read_vol(fname)

  if (!is.null(outspace)) {
    atlas <- resample(atlas, outspace)
  }

  ids <- sort(unique(as.vector(atlas))[-1])
  labels <- c(
    "Thalamus",
    "Caudate",
    "Putamen",
    "Pallidum",
    "Brainstem",
    "Hippocampus",
    "Amygdala",
    "Accumbens",
    "VentralDC",
    "Thalamus",
    "Caudate",
    "Putamen",
    "Pallidum",
    "Hippocampus",
    "Amygdala",
    "Accumbens",
    "VentralDC")

  hemi=c(rep("left", 4), NA, rep("left", 3), rep("right", 8))
  cmap <- tibble::tribble(
    ~red, ~green, ~blue,
    0,   118, 14,
    122, 186, 220,
    236, 13,  176,
    12,  48,  255,
    119, 159, 176,
    220, 216, 20,
    220, 216, 20,
    255, 165, 0,
    165, 42,  42,
    0,   118, 14,
    122, 186, 220,
    236, 13,  176,
    13,  48,  255,
    220, 216, 20,
    103, 255, 255,
    255, 165, 0,
    165, 42,  42)

  ret <- list(
    atlas=atlas,
    cmap=cmap,
    ids=ids,
    labels=labels,
    hemi=hemi)

  class(ret) <- c("aseg", "atlas")
  ret
}
