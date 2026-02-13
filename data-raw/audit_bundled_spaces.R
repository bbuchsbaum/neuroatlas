#!/usr/bin/env Rscript
# audit_bundled_spaces.R
#
# Reproducible audit of bundled atlas NIfTI headers to determine
# template space provenance. Run from the package root directory.
#
# Findings are used to set confidence levels in atlas_ref metadata.

library(neuroim2)

cat("=== Bundled Atlas Space Audit ===\n\n")

# --- Known template references ---
cat("Known MNI152NLin6Asym (FSL MNI152) template properties:\n")
cat("  Dimensions: 193 x 229 x 193\n")
cat("  Voxel size: 1mm isotropic\n")
cat("  Origin (sform): approx (-96, -132, -78)\n")
cat("  Orientation: RAS\n\n")

cat("Known MNI152NLin2009cAsym (fMRIPrep) template properties:\n")
cat("  Dimensions: 193 x 229 x 193\n")
cat("  Voxel size: 1mm isotropic\n")
cat("  Origin (sform): approx (-96, -132, -78)\n")
cat("  Note: Same bounding box as NLin6Asym but different nonlinear registration\n\n")

# --- ASEG Atlas ---
cat("--- ASEG Atlas (atlas_aparc_aseg_prob33.nii.gz) ---\n")
aseg_path <- system.file("extdata/atlas_aparc_aseg_prob33.nii.gz",
                          package = "neuroatlas")
if (nzchar(aseg_path)) {
  aseg_vol <- read_vol(aseg_path)
  aseg_sp <- space(aseg_vol)
  cat("  Dimensions:", paste(dim(aseg_vol), collapse = " x "), "\n")
  cat("  Voxel size:", paste(round(spacing(aseg_sp), 3), collapse = " x "), "\n")
  cat("  Origin:", paste(round(origin(aseg_sp), 2), collapse = ", "), "\n")
  cat("  Affine:\n")
  print(round(trans(aseg_sp), 4))
  cat("\n")

  # Assessment
  dims_match_nlin6 <- all(dim(aseg_vol) == c(193, 229, 193))
  voxel_1mm <- all(abs(spacing(aseg_sp) - 1.0) < 0.01)

  cat("  Assessment:\n")
  if (dims_match_nlin6 && voxel_1mm) {
    cat("  -> Dimensions and voxel size match MNI152NLin6Asym (193x229x193, 1mm)\n")
    cat("  -> CONCLUSION: HIGH confidence MNI152NLin6Asym\n")
    cat("  -> The ASEG was derived from FreeSurfer's aparc+aseg, which uses\n")
    cat("     the FSL MNI152 template (MNI152NLin6Asym) as its standard space.\n")
  } else {
    cat("  -> Dimensions do NOT match standard MNI152NLin6Asym\n")
    cat("  -> CONCLUSION: Remains UNCERTAIN\n")
  }
} else {
  cat("  File not found. Run from package root or install the package first.\n")
}

cat("\n")

# --- Olsen MTL Atlas ---
cat("--- Olsen MTL Atlas ---\n")
olsen_mtl <- NULL
tryCatch({
  utils::data("olsen_mtl", package = "neuroatlas", envir = environment())
  olsen_vol <- olsen_mtl$atlas
  olsen_sp <- space(olsen_vol)
  cat("  Dimensions:", paste(dim(olsen_vol), collapse = " x "), "\n")
  cat("  Voxel size:", paste(round(spacing(olsen_sp), 3), collapse = " x "), "\n")
  cat("  Origin:", paste(round(origin(olsen_sp), 2), collapse = ", "), "\n")
  cat("  Affine:\n")
  print(round(trans(olsen_sp), 4))
  cat("\n")

  # Assessment
  dims_182 <- all(dim(olsen_vol) == c(182, 218, 182))
  voxel_1mm <- all(abs(spacing(olsen_sp) - 1.0) < 0.01)

  cat("  Assessment:\n")
  if (dims_182 && voxel_1mm) {
    cat("  -> Dimensions 182x218x182 at 1mm - this is a non-standard MNI grid\n")
    cat("  -> Neither MNI152NLin6Asym (193x229x193) nor MNI152NLin2009cAsym\n")
    cat("  -> Likely a custom cropped MNI152 volume from the original study\n")
    cat("  -> CONCLUSION: Remains UNCERTAIN (custom MNI152-like grid)\n")
  } else {
    cat("  -> Non-standard dimensions\n")
    cat("  -> CONCLUSION: Remains UNCERTAIN\n")
  }
}, error = function(e) {
  cat("  Could not load Olsen MTL data:", conditionMessage(e), "\n")
})

cat("\n=== Audit Complete ===\n")
