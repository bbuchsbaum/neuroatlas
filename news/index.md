# Changelog

## neuroatlas 0.1.0.9000

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
