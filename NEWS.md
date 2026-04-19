# neuroatlas 0.1.0.9000

* Added a canonical `new_atlas()` / `new_surfatlas()` constructor that
  assembles every loader's return value (Schaefer, Glasser, ASEG,
  Olsen MTL / hippocampus, TemplateFlow subcortical). The constructor
  validates required fields with a typed `neuroatlas_error_invalid_atlas`
  condition, normalises RGB colour maps to a data frame, builds
  `roi_metadata` uniformly, and attaches `atlas_ref` / provenance in one
  place — removing ~100 lines of per-loader boilerplate.
* Added a lightweight atlas registry (`register_atlas()`) exposed via two
  new public helpers: `list_atlases()` enumerates the built-in atlases,
  and `get_atlas(name, ...)` dispatches to the registered loader by id or
  alias (e.g. `get_atlas("schaefer2018", parcels="100", networks="7")`).
* Added centralised download helpers (`.neuroatlas_download()`,
  `.neuroatlas_try_download()`) used by the Schaefer and Glasser loaders.
  Failures now raise classed `neuroatlas_error_download` conditions with
  the upstream URL instead of returning a silent `NULL`; Git LFS pointer
  stubs are detected and reported explicitly.
* Atlas loaders now emit `cli::cli_abort()` / `cli::cli_warn()` with
  structured classes (`neuroatlas_error_*`, `neuroatlas_warn_*`) in place
  of bare `stop()` / `warning()`, so callers can catch loader errors by
  class.
* Added atlas provenance descriptors via new `atlas_ref` infrastructure:
  `new_atlas_ref()`, `atlas_ref()`, `atlas_family()`, `atlas_space()`,
  `atlas_coord_space()`, and `validate_atlas_ref()`.
* Atlas constructors now attach structured provenance/space metadata and
  compatibility aliases (`space`, `template_space`, `coord_space`,
  `confidence`) for Schaefer, Glasser, ASEG, Olsen MTL/hippocampus, and
  TemplateFlow subcortical atlases.
* `get_glasser_atlas()` now accepts a `source` argument and defaults to
  `source = "mni2009c"` with fallback to legacy `xcpengine` when unavailable.
  Fallback paths are tagged with `confidence = "uncertain"`.
* Added `test-atlas-ref.R` coverage for atlas reference metadata and basic
  cross-representation label concordance checks.
* Added space-level transform planning utilities backed by
  `inst/extdata/transform_registry.csv`: `space_transform_manifest()`,
  `atlas_transform_plan()`, and scope-aware `atlas_transform_manifest()`.
* `atlas_alignment()` now consults the space transform registry for
  same-representation cross-template routes (e.g., NLin6Asym to 2009cAsym)
  and reports route-specific status/confidence.
* Fixed white gaps ("shards") in `plot_brain()` surface rendering caused by
  inconsistent triangle winding in some meshes.
* Added `silhouette*` and `network_border*` options to `plot_brain()` for
  improved boundary styling (silhouette outline and between-network borders).
* Improved `plot_brain()` aesthetics with smoother boundary rendering
  (`border_geom = "path"`) and an optional normal-based shading overlay
  (`shading*`, `fill_alpha`).

# neuroatlas 0.1.0

* Initial CRAN submission
* Added support for multiple neuroimaging atlases:
  - Schaefer cortical parcellations (100-1000 parcels, 7/17 networks)
  - Glasser multi-modal parcellation (360 regions)
  - FreeSurfer ASEG subcortical segmentation
  - Olsen medial temporal lobe atlas
* Integrated TemplateFlow support for standardized templates
* Added visualization support via ggseg and echarts4r
* Implemented atlas operations: ROI extraction, data reduction, resampling
* Added comprehensive vignettes and documentation
