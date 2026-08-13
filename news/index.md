# Changelog

## neuroatlas 0.1.0.9000

- [`dilate_atlas()`](../reference/dilate_atlas.md) now genuinely honours
  its `radius` argument. The previous implementation passed a fixed `k`
  to `Rnanoflann::nn(search = "radius")`, which returns the `k` nearest
  neighbours regardless of distance, so `radius` had no effect and
  dilation filled the entire mask (absorbing, for a cortical atlas,
  distant cerebellar and deep subcortical grey matter). Dilation now
  uses a standard k-NN search with an explicit Euclidean radius cutoff:
  in-mask voxels with no parcel within `radius` voxels are left
  unassigned. This is a behaviour change — callers that relied on the
  old whole-mask fill will now get radius-limited results. See the new
  “Dilating an Atlas to Cover Grey Matter” vignette.
- Added
  [`get_harvard_oxford_atlas()`](../reference/get_harvard_oxford_atlas.md)
  and registry entries for Harvard-Oxford cortical, subcortical, and
  combined structural parcellations. The default source is TemplateFlow,
  with threshold and resolution options for maximum-probability `dseg`
  images.
- Added [`get_fsl_atlas()`](../reference/get_fsl_atlas.md) for FSL
  XML-described atlases, including the documented offset between
  probabilistic XML label indices and max-probability summary image
  label values. Added a thin FSL-backed wrapper for Julich-Brain /
  Brodmann-style cytoarchitectonic labels
  ([`get_julich_brain_atlas()`](../reference/get_julich_brain_atlas.md)),
  which now downloads the Nilearn/NITRC `Juelich.tgz` archive into a
  local FSL-style cache when `FSLDIR` is unset.
- `plot_brain(overlay = <NeuroVol>)` now propagates missing data through
  the volume-to-surface projection: vertices that fall outside the input
  volume’s coverage (or whose neighbourhood contains no finite source
  voxel) are emitted as `NA` rather than `0`. Faces with no finite
  vertices are dropped from the polygon set, so uncovered cortex renders
  as transparent background instead of an opaque dark-palette wash.
  Faces with partial coverage continue to render using the average of
  their finite vertices. The internal `vol_to_surf()` `fill` argument
  changed from `0` to `NA_real_`.
- `plot_brain(overlay = <NeuroVol>)` now repairs legacy
  `SurfaceGeometry` objects on the fly. The bundled `data(fsaverage)`
  artefact and the `@geometry` slots inside packaged surface atlases
  were serialized before
  [`neurosurf::SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry.html)
  gained the `label` and `surf_to_world` slots; accessing those slots on
  a legacy object errored out and caused `vol_to_surf()` to silently
  return all-NA overlays. `.resolve_overlay_surface_pair()` now rebuilds
  any geometry that fails `validObject()` via the current constructor
  before passing it to `vol_to_surf()`.
- Added a canonical [`new_atlas()`](../reference/atlas_constructor.md) /
  [`new_surfatlas()`](../reference/atlas_constructor.md) constructor
  that assembles every loader’s return value (Schaefer, Glasser, ASEG,
  Olsen MTL / hippocampus, TemplateFlow subcortical). The constructor
  validates required fields with a typed
  `neuroatlas_error_invalid_atlas` condition, normalises RGB colour maps
  to a data frame, builds `roi_metadata` uniformly, and attaches
  `atlas_ref` / provenance in one place — removing ~100 lines of
  per-loader boilerplate.
- Added a lightweight atlas registry
  ([`register_atlas()`](../reference/register_atlas.md)) exposed via two
  new public helpers: [`list_atlases()`](../reference/list_atlases.md)
  enumerates the built-in atlases, and `get_atlas(name, ...)` dispatches
  to the registered loader by id or alias
  (e.g. `get_atlas("schaefer2018", parcels="100", networks="7")`).
- Added centralised download helpers
  ([`.neuroatlas_download()`](../reference/dot-neuroatlas_download.md),
  [`.neuroatlas_try_download()`](../reference/dot-neuroatlas_try_download.md))
  used by the Schaefer and Glasser loaders. Failures now raise classed
  `neuroatlas_error_download` conditions with the upstream URL instead
  of returning a silent `NULL`; Git LFS pointer stubs are detected and
  reported explicitly.
- Atlas loaders now emit
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) /
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)
  with structured classes (`neuroatlas_error_*`, `neuroatlas_warn_*`) in
  place of bare [`stop()`](https://rdrr.io/r/base/stop.html) /
  [`warning()`](https://rdrr.io/r/base/warning.html), so callers can
  catch loader errors by class.
- Added atlas provenance descriptors via new `atlas_ref` infrastructure:
  [`new_atlas_ref()`](../reference/new_atlas_ref.md),
  [`atlas_ref()`](../reference/atlas_ref.md),
  [`atlas_family()`](../reference/atlas_family.md),
  [`atlas_space()`](../reference/atlas_space.md),
  [`atlas_coord_space()`](../reference/atlas_coord_space.md), and
  [`validate_atlas_ref()`](../reference/validate_atlas_ref.md).
- Atlas constructors now attach structured provenance/space metadata and
  compatibility aliases (`space`, `template_space`, `coord_space`,
  `confidence`) for Schaefer, Glasser, ASEG, Olsen MTL/hippocampus, and
  TemplateFlow subcortical atlases.
- [`get_glasser_atlas()`](../reference/get_glasser_atlas.md) now accepts
  a `source` argument and defaults to `source = "mni2009c"` with
  fallback to legacy `xcpengine` when unavailable. Fallback paths are
  tagged with `confidence = "uncertain"`.
- Added `test-atlas-ref.R` coverage for atlas reference metadata and
  basic cross-representation label concordance checks.
- Added space-level transform planning utilities backed by
  `inst/extdata/transform_registry.csv`:
  [`space_transform_manifest()`](../reference/space_transform_manifest.md),
  [`atlas_transform_plan()`](../reference/atlas_transform_plan.md), and
  scope-aware
  [`atlas_transform_manifest()`](../reference/atlas_transform_manifest.md).
- [`atlas_alignment()`](../reference/atlas_alignment.md) now consults
  the space transform registry for same-representation cross-template
  routes (e.g., NLin6Asym to 2009cAsym) and reports route-specific
  status/confidence.
- Fixed white gaps (“shards”) in
  [`plot_brain()`](../reference/plot_brain.md) surface rendering caused
  by inconsistent triangle winding in some meshes.
- Added `silhouette*` and `network_border*` options to
  [`plot_brain()`](../reference/plot_brain.md) for improved boundary
  styling (silhouette outline and between-network borders).
- Improved [`plot_brain()`](../reference/plot_brain.md) aesthetics with
  smoother boundary rendering (`border_geom = "path"`) and an optional
  normal-based shading overlay (`shading*`, `fill_alpha`).

## neuroatlas 0.1.0

- Initial CRAN submission
- Added support for multiple neuroimaging atlases:
  - Schaefer cortical parcellations (100-1000 parcels, 7/17 networks)
  - Glasser multi-modal parcellation (360 regions)
  - FreeSurfer ASEG subcortical segmentation
  - Olsen medial temporal lobe atlas
- Integrated TemplateFlow support for standardized templates
- Added visualization support via ggseg and echarts4r
- Implemented atlas operations: ROI extraction, data reduction,
  resampling
- Added comprehensive vignettes and documentation
