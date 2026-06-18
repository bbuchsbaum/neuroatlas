# Generates the pre-rendered figures embedded in vignettes/atlas-dilation.Rmd.
#
# Heavy + network-dependent (TemplateFlow downloads, ~2 min dilation), so it is
# run by hand and the resulting PNGs are committed under vignettes/figures/.
#
#   Rscript data-raw/dilation-vignette-figures.R
#
# The expensive step (resampling + dilation) is cached to a temporary RDS so
# re-runs that only tweak the rendering are fast. Delete the cache to recompute.
#
# Requires the `templateflow` R package (remotes::install_github("bbuchsbaum/templateflow")).

suppressWarnings(suppressMessages({
  devtools::load_all(quiet = TRUE)
  library(neuroim2)
}))

out_dir <- "vignettes/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cache <- "/tmp/neuroatlas-dilation-cache.rds"  # stable across runs; safe to delete
radius <- 4          # voxels (~8 mm at 2 mm resolution)
radii <- c(1, 4, 6)  # radii shown in the before/after ROI comparison
gm_thresh <- 0.33
zlevels <- c(24, 40, 56)
roi_compare_z <- 40

if (file.exists(cache)) {
  message("loading cached dilation from ", cache)
  d <- readRDS(cache)
} else {
  # --- inputs --------------------------------------------------------------
  sch <- get_schaefer_atlas("400", "7", "2")                    # MNI152NLin6Asym, 2 mm
  t1  <- get_template(space = "MNI152NLin6Asym", variant = "brain",
                      modality = "T1w", resolution = 2)
  gm  <- get_template(space = "MNI152NLin2009cAsym", variant = "probseg",
                      label = "GM", resolution = 2)             # GM probseg lives here only

  sp     <- neuroim2::space(sch$atlas)
  gm_rs  <- resample(gm, sp)
  t1_rs  <- resample(t1, sp)
  mask   <- neuroim2::LogicalNeuroVol(gm_rs > gm_thresh, space = sp)

  message("dilating (radius = ", radius, ") ...")
  tt  <- system.time(dil <- dilate_atlas(sch, mask, radius = radius, maxn = 50))
  cat(sprintf("dilate time: %.0fs\n", tt[3]))

  d <- list(
    dims = dim(sch$atlas),
    t1   = as.vector(t1_rs),
    av   = as.vector(neuroim2::as.dense(sch$atlas)),
    dv   = as.vector(neuroim2::as.dense(dil$atlas)),
    gmv  = as.vector(gm_rs) > gm_thresh,
    sp   = sp
  )
  saveRDS(d, cache)
}

if (is.null(d$dv_by_radius)) d$dv_by_radius <- list()
if (!is.null(d$dv)) d$dv_by_radius[[as.character(radius)]] <- d$dv

missing_radii <- setdiff(as.character(radii), names(d$dv_by_radius))
if (length(missing_radii) > 0) {
  sch <- get_schaefer_atlas("400", "7", "2")
  mask <- neuroim2::LogicalNeuroVol(array(d$gmv, dim = d$dims),
                                    space = d$sp)
  for (rr in as.integer(missing_radii)) {
    message("dilating missing radius = ", rr, " ...")
    tt <- system.time({
      dil <- dilate_atlas(sch, mask, radius = rr, maxn = 50)
    })
    cat(sprintf("radius %d dilate time: %.0fs\n", rr, tt[3]))
    d$dv_by_radius[[as.character(rr)]] <-
      as.vector(neuroim2::as.dense(dil$atlas))
  }
  saveRDS(d, cache)
}

av <- d$av
dv <- d$dv_by_radius[[as.character(radius)]]
gmv <- d$gmv
sp <- d$sp
dims <- d$dims
t1_rs <- neuroim2::NeuroVol(array(d$t1, dim = dims), space = sp)

# --- reported numbers ------------------------------------------------------
cat(sprintf("before labeled: %d | after labeled: %d | added: %d\n",
            sum(av > 0), sum(dv > 0), sum(dv > 0) - sum(av > 0)))
cat(sprintf("GM>%.2f total: %d | covered before: %d (%.1f%%) | after: %d (%.1f%%)\n",
            gm_thresh, sum(gmv), sum(gmv & av > 0), 100 * mean((av > 0)[gmv]),
            sum(gmv & dv > 0), 100 * mean((dv > 0)[gmv])))
cat(sprintf("GM>%.2f left unassigned by the radius guard: %d\n",
            gm_thresh, sum(gmv & dv == 0)))

# Helper: NA background so the T1 shows through; integer codes elsewhere.
code_vol <- function(code) {
  code[code == 0] <- NA_real_
  neuroim2::NeuroVol(array(code, dim = dims), space = sp)
}

roi_vol <- function(code) {
  neuroim2::NeuroVol(array(ifelse(code > 0, code, NA_real_), dim = dims),
                     space = sp)
}

covered_pct <- function(code) {
  sprintf("%.1f%%", 100 * mean((code > 0)[gmv]))
}

# --- figure 1: original parcels + dilated additions ------------------------
add_code <- ifelse(dv > 0 & av == 0, 2, ifelse(av > 0, 1, 0))
png(file.path(out_dir, "dilation-additions.png"),
    width = 1500, height = 560, res = 130)
plot_overlay(t1_rs, code_vol(add_code), zlevels = zlevels, along = 3L,
             ov_cmap = "viridis", ov_range = c(0, 2), ov_alpha = 0.85,
             ov_thresh = 0, ncol = 3,
             title = "GM-only dilation: cortical rim plus nearby non-cortical spillover, radius = 4")
dev.off()

# --- figure 2: ROI labels before and after increasing dilation radii --------
roi_range <- c(1, max(av, unlist(d$dv_by_radius), na.rm = TRUE))
roi_panels <- list(
  plot_overlay(t1_rs, roi_vol(av), zlevels = roi_compare_z, along = 3L,
               ov_cmap = "viridis", ov_range = roi_range, ov_alpha = 0.9,
               ov_thresh = 0, ncol = 1, colorbar = FALSE, draw = FALSE,
               title = paste0("Original\n", covered_pct(av))),
  plot_overlay(t1_rs, roi_vol(d$dv_by_radius[["1"]]), zlevels = roi_compare_z,
               along = 3L, ov_cmap = "viridis", ov_range = roi_range,
               ov_alpha = 0.9, ov_thresh = 0, ncol = 1, colorbar = FALSE,
               draw = FALSE, title = paste0("radius = 1\n",
                                             covered_pct(d$dv_by_radius[["1"]]))),
  plot_overlay(t1_rs, roi_vol(d$dv_by_radius[["4"]]), zlevels = roi_compare_z,
               along = 3L, ov_cmap = "viridis", ov_range = roi_range,
               ov_alpha = 0.9, ov_thresh = 0, ncol = 1, colorbar = FALSE,
               draw = FALSE, title = paste0("radius = 4\n",
                                             covered_pct(d$dv_by_radius[["4"]]))),
  plot_overlay(t1_rs, roi_vol(d$dv_by_radius[["6"]]), zlevels = roi_compare_z,
               along = 3L, ov_cmap = "viridis", ov_range = roi_range,
               ov_alpha = 0.9, ov_thresh = 0, ncol = 1, colorbar = FALSE,
               draw = FALSE, title = paste0("radius = 6\n",
                                             covered_pct(d$dv_by_radius[["6"]])))
)
roi_plot <- patchwork::wrap_plots(roi_panels, ncol = 4) +
  patchwork::plot_annotation(
    title = paste0("Schaefer ROIs before and after dilation (z = ",
                   roi_compare_z, ")"),
    subtitle = "Same parcel labels, increasing radius; percentages are coverage of the p > 0.33 mask"
  )
ggplot2::ggsave(file.path(out_dir, "dilation-radius-rois.png"), roi_plot,
                width = 1500 / 130, height = 520 / 130, dpi = 130,
                bg = "white")

# --- figure 2: the proximity guard (GM left unassigned) --------------------
guard_code <- ifelse(gmv & dv == 0, 1, 0)
png(file.path(out_dir, "dilation-guard.png"),
    width = 1500, height = 560, res = 130)
plot_overlay(t1_rs, code_vol(guard_code), zlevels = zlevels, along = 3L,
             ov_cmap = "viridis", ov_range = c(0, 1), ov_alpha = 0.95,
             ov_thresh = 0, ncol = 3,
             title = "Grey matter beyond the proximity radius remains unassigned")
dev.off()

message("wrote figures to ", out_dir)
