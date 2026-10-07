# Regenerate the nonlinear before/after figure from the vignette's own code.
# Run from the repository root with the released forward H5 already cached:
# Rscript data-raw/template-transform-vignette-figure.R /path/to/transform-cache
# TemplateFlow's two 2 mm brain templates must also be cached.
# Input licenses: see the transform-artifacts-v1 release's LICENSES.md.

devtools::load_all(quiet = TRUE)
stopifnot(
  requireNamespace("ragg", quietly = TRUE),
  requireNamespace("patchwork", quietly = TRUE)
)
args <- commandArgs(trailingOnly = TRUE)
cache_dir <- if (length(args)) args[[1L]] else transform_cache_path()
nonlinear <- get_template_transform(
  "MNI152NLin6Asym", "MNI152NLin2009cAsym", provider = "neuroatlas",
  cache_dir = cache_dir, offline = TRUE
)

vignette <- readLines("vignettes/template-transforms.Rmd")
start <- match("```{r nonlinear-visual-code, eval=FALSE}", vignette)
stopifnot(!is.na(start))
end <- start + match("```", vignette[seq.int(start + 1L, length(vignette))])
code <- vignette[seq.int(start + 1L, end - 1L)]
figure_path <- "vignettes/figures/template-transform-before-after.png"
ragg::agg_png(
  figure_path, width = 10, height = 8.4, units = "in", res = 200,
  background = "white"
)
tryCatch(eval(parse(text = code)), finally = grDevices::dev.off())

receipt <- attr(after, "neuroatlas_transform")
stopifnot(
  identical(neuroim2::space(before), neuroim2::space(target_t1)),
  identical(neuroim2::space(after), neuroim2::space(target_t1)),
  identical(receipt$from_space, "MNI152NLin6Asym"),
  identical(receipt$to_space, "MNI152NLin2009cAsym"),
  identical(receipt$data_type, "continuous"),
  identical(receipt$interpolation, "linear"),
  all(is.finite(as.vector(after))),
  !isTRUE(all.equal(as.vector(before), as.vector(after)))
)
receipt$files <- basename(receipt$files)

hash_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}
input_record <- function(template) {
  path <- get_template(
    template, variant = "brain", resolution = 2, path_only = TRUE
  )
  list(file = basename(path), sha256 = hash_file(path))
}
versions <- vapply(
  c("neuroatlas", "neuroim2", "neurotransform", "patchwork", "ragg"),
  function(package) as.character(utils::packageVersion(package)), character(1)
)
binding_path <- Sys.getenv("NEUROATLAS_ENGINE_BINDING", "")
engine_build <- if (nzchar(binding_path)) {
  binding <- jsonlite::read_json(binding_path)
  binding[c("engine_revision", "archive_sha256", "version")]
} else {
  NULL
}
jsonlite::write_json(
  list(
    figure = list(
      file = basename(figure_path), sha256 = hash_file(figure_path)
    ),
    source = input_record("MNI152NLin6Asym"),
    target = input_record("MNI152NLin2009cAsym"),
    receipt = receipt,
    display = list(
      axial_mm = c(-12, 20, 52), tile_voxels = 8L,
      target_range = target_range, source_range = source_range,
      match_intensity = FALSE, width_inches = 10, height_inches = 8.4, dpi = 200
    ),
    package_versions = as.list(versions),
    engine_build = engine_build
  ),
  "data-raw/template-transform-vignette-figure.json",
  pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null"
)
