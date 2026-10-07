# Localize whether lost parcels have any eligible final-surface contributors.
# Rscript data-raw/projection-diagnostics-v1/support.R QUALITY_WORK EXPORT OUTPUT
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L, !file.exists(args[[3L]]))
work <- args[[1L]]
exported <- args[[2L]]
devtools::load_all(quiet = TRUE)
rows <- list()
for (hemi in c("L", "R")) {
  source <- get_surface_geometry("fsaverage", "164k", hemi,
    cache_dir = file.path(work, "cache"), offline = TRUE)
  target <- get_surface_geometry("fsLR", "32k", hemi,
    cache_dir = file.path(work, "cache"), offline = TRUE)
  operator <- get_surface_transform(source, target,
    cache_dir = file.path(work, "cache"))
  plan <- operator$plan
  sampled <- readBin(file.path(exported, paste0(hemi, "-1000-sampled.bin")),
    "double", n = source$domain$n_vertices, size = 8L, endian = "little")
  available <- as.logical(readBin(file.path(exported,
    paste0(hemi, "-1000-aggregate-available.bin")), "integer",
    n = target$domain$n_vertices, size = 4L, endian = "little"))
  diagnostics <- utils::read.csv(file.path(exported,
    paste0(hemi, "-1000-labels.csv")))
  for (key in diagnostics$key[diagnostics$lost_at_resampling]) {
    edges <- sampled[plan$cols] == key & plan$vals > 0
    edges[is.na(edges)] <- FALSE
    cortex_edges <- edges & target$cortex[plan$rows]
    supported_edges <- cortex_edges & available[plan$rows]
    target_rows <- unique(plan$rows[supported_edges])
    rows[[length(rows) + 1L]] <- data.frame(
      hemisphere = hemi, key = key,
      sampled_vertices = sum(sampled == key, na.rm = TRUE),
      contributing_target_cortex_vertices = length(unique(plan$rows[cortex_edges])),
      contributing_supported_target_vertices = length(target_rows),
      contributing_masked_target_vertices = length(unique(plan$rows[
        edges & !target$cortex[plan$rows]])),
      supported_weight_mass = sum(plan$vals[supported_edges]),
      operator_id = operator$integrity
    )
  }
}
utils::write.csv(do.call(rbind, rows), args[[3L]], row.names = FALSE)
