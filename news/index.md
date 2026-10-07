# Changelog

## neuroatlas 0.2.0.9000

- Accept neurosurf’s `0.1.0` CRAN-preparation version and pin its tested
  GitHub revision. This fixes CI dependency resolution after upstream
  release numbering replaced the higher-numbered development version.

- Exact pinned fsaverage 164k surfaces support directed native
  resampling to and from fsaverage6 (41k) and fsaverage5 (10k),
  separately for each hemisphere. New input manifests preserve released
  domain identities. Qualification covers numerical interpolation and
  data policies; resampling remains lossy and does not establish
  anatomical accuracy, area conservation or an inverse.
  [`space_transform_manifest()`](https://bbuchsbaum.github.io/neuroatlas/reference/space_transform_manifest.md)
  exposes route scope and surface densities, and the transform vignette
  separates executable routes from roadmap placeholders.

- [`projection_diagnostics()`](https://bbuchsbaum.github.io/neuroatlas/reference/projection_diagnostics.md)
  reports per-map cortical coverage and per-key source, sampled-surface
  and target counts. It distinguishes absent source keys, sampling loss
  and surface-resampling loss. Explicit atlas or label-table hemisphere
  declarations expose wrong-hemisphere output vertices. Diagnostics
  preserve values, supported zero and missingness; no label repair is
  applied.

- CIFTI-2 dense scalar and label maps preserve cortical and volumetric
  brain models, ordered indices, voxel geometry, map metadata and
  per-map label tables through
  [`read_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/read_cifti.md)
  and
  [`write_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/write_cifti.md),
  using optional RNifti and xml2.
  [`replace_cifti_values()`](https://bbuchsbaum.github.io/neuroatlas/reference/replace_cifti_values.md)
  validates replacement values without changing layouts. Previously
  unavailable samples remain unavailable after value replacement.

- [`get_cifti_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_cifti_transform.md)
  and the template API bind explicitly supplied qualified cortical
  operators to CIFTI layouts. Application preserves noncortical values
  by structure/index and rejects changed voxel geometry, support or
  declared template frames. Unsupported label samples require explicit
  unassigned keys; unavailable rows remain recorded in map metadata and
  availability masks. This adapter does not implement cross-frame volume
  warping, directed rasterization, new surface densities or anatomical
  qualification.

## neuroatlas 0.2.0

- [`get_surface_geometry()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_surface_geometry.md)
  fetches checksum-locked fsaverage 164k and fsLR 32k registration
  geometry, masks and vertex areas. Exact domains admit directed native
  surface routes through
  [`get_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_template_transform.md)
  and
  [`apply_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_template_transform.md);
  broad surface aliases remain planned.

- [`get_surface_projection()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_surface_projection.md)
  adds population cortical projection from exact MNI152NLin6Asym and
  MNI152NLin2009cAsym templates.
  [`transform_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/transform_atlas.md)
  accepts an exact cortical destination and retains source atlas
  identity and labels. Probability channels keep partial mass;
  unsupported samples remain missing.
  [`ribbon_projection()`](https://bbuchsbaum.github.io/neuroatlas/reference/ribbon_projection.md)
  provides separate caller-aligned white/pial sampling. Numerical
  qualification is scoped to pinned inputs, methods and grids.

- Verified surface inputs reuse the transform cache with checksum
  validation, offline replay and selective cleanup. The native engine is
  pinned to its qualified revision; neuroim2 \>= 0.19.0 is now required.

- Generic MNI305/MNI152 coordinate affines are described as approximate
  family transforms rather than exact template correspondence.

- Package CI uses neuroatlas’s coding conventions, generated
  documentation, package checks, website builds and coverage
  measurement. Complexity review and rOpenSci recommendations remain
  advisory. The unused eco-atlas workflow runs only on explicit request.

- [`query_point()`](https://bbuchsbaum.github.io/neuroatlas/reference/query_point.md)
  gains `nearest = TRUE`, which returns one row per point and atlas: the
  containing region at distance 0, otherwise the closest labelled voxel
  within `radius` mm, with the distance in a new `distance` column.
  Equidistant ties go to the smallest region id.
  [`query_coord()`](https://bbuchsbaum.github.io/neuroatlas/reference/query_coord.md)
  and
  [`query_vox()`](https://bbuchsbaum.github.io/neuroatlas/reference/query_coord.md)
  pass it through `...`; the default output is unchanged.

- New exported
  [`project_cluster_overlay()`](https://bbuchsbaum.github.io/neuroatlas/reference/project_cluster_overlay.md)
  projects a `NeuroVol` overlay onto a surface atlas’s vertices and
  returns the per-vertex values
  [`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
  draws (`list(overlay = list(lh, rh), meta)`), so callers no longer
  need the internal `.project_cluster_overlay()`. A hemisphere that
  cannot be projected now raises a warning and records the reason in
  `meta$hemis[[hemi]]$error` instead of silently becoming all `NA`.

- `plot(<atlas>, view = "ortho")` works again with neuroim2 \>= 0.19,
  whose redesigned `plot_ortho()` returns one assembled figure by
  default. The per-plane panels are now requested with
  `assemble = FALSE`, drawn without raster interpolation (which blended
  neighbouring region colours) or head cropping, and region-ID mapping
  tolerates non-numeric slice values. Older neuroim2 releases are
  handled unchanged.

- Glasser volume and surface atlases now share hemisphere-qualified
  `label_full` and `area` metadata. Their existing, opposite ID
  conventions are recorded in atlas and parcel metadata. Glasser table
  joins on `id` require an explicit matching `id_convention`, preventing
  silent hemisphere swaps. Use `label_full` plus value columns to
  transfer results between representations.

- CPU surface figures
  (`plot_brain(static_backend = "cpu", orientation_labels = TRUE)`)
  place the anterior/posterior marks from the view’s camera, not the
  hemisphere alone. Medial panels had A and P swapped, and ventral
  panels drew anterior at the top.

- [`surface_anatomy()`](https://bbuchsbaum.github.io/neuroatlas/reference/surface_anatomy.md)
  gains `type = "sulcal_depth"`, a FreeSurfer-style sulcal-depth proxy
  from the white and displayed (inflated) surfaces. It gives the broad
  two-tone gyral/sulcal underlay used by Workbench and pycortex; the
  default (`"curvature"`) is unchanged, and explicit or atlas metrics
  still win.

- [`merge_atlases()`](https://bbuchsbaum.github.io/neuroatlas/reference/merge_atlases.md)
  keeps every per-region column of both parents (e.g. Schaefer
  `network`), filling `NA` for regions from a parent that lacks it, so
  [`roi_metadata()`](https://bbuchsbaum.github.io/neuroatlas/reference/roi_metadata.md)
  and
  [`query_point()`](https://bbuchsbaum.github.io/neuroatlas/reference/query_point.md)
  on a composite report them. It also remaps the second atlas’s ids in a
  single lookup: previously a voxel could be remapped twice (with ASEG
  first, Schaefer parcel 38 was reported as another parcel).

- [`query_point()`](https://bbuchsbaum.github.io/neuroatlas/reference/query_point.md)
  no longer errors on atlases whose `coord_space` is `NA`, empty or
  `"Unknown"`, such as
  [`merge_atlases()`](https://bbuchsbaum.github.io/neuroatlas/reference/merge_atlases.md)
  composites of parents in different spaces. They are queried in their
  own world coordinates without a transform, with a warning of class
  `"neuroatlas_unknown_coord_space"`.

## neuroatlas 0.1.0.9005

- The CPU parcel renderer again uses the sulcal-depth underlay on
  inflated fsaverage6 atlases (e.g. Schaefer): a declared atlas density
  no longer diverts the white-mesh lookup away from the packaged meshes,
  which had fallen back to raw curvature.
- Parcel-map colorbars label the limits, threshold, and zero without
  crowding, set the threshold ticks in bold, and are wider.

## neuroatlas 0.1.0.9004

- `plot_brain(static_backend = "cpu", vals = ...)` now renders
  publication parcel maps with
  [`neurosurf::render_surface_parcels()`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_parcels.html):
  smooth antialiased parcel edges, a sulcal-depth underlay with soft
  lighting, a flat medial wall drawn only in medial views, and a figure
  whose layout (2x2 grid or single row, colorbar below or beside) adapts
  to the device. New arguments `vals_threshold` (unfills sub-threshold
  parcels and greys that band of the colorbar) and `parcel_style`. The
  CPU backend previously rejected `vals`.

## neuroatlas 0.1.0.9003

- Enabled the qualified `MNI152NLin6Asym` / `MNI152NLin2009cAsym` ANTs
  transform pair in both directions, with published artifact hashes,
  input and software provenance, and qualification on 1 mm and 2 mm
  target grids.
- Added
  [`get_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_template_transform.md),
  [`apply_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_template_transform.md),
  and
  [`transform_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/transform_atlas.md)
  for explicit template-space routes and target grids. Routes compose
  before a single resampling pass. Label IDs and metadata are retained,
  probability channels are never renormalized, and provenance records
  the artifact hashes, grids, and interpolation.
- Added verified transform caching with atomic downloads, offline
  operation, and cache cleanup restricted to neuroatlas-owned files. The
  route planner handles longer paths and can restrict resolution to
  executable releases.

## neuroatlas 0.1.0.9000

- New
  [`surface_anatomy()`](https://bbuchsbaum.github.io/neuroatlas/reference/surface_anatomy.md)
  resolves the sulcal shading metric shared by the CPU surface
  renderers: mean curvature of the matching white mesh, used only when
  its topology matches the display mesh, with explicit provenance.
  Default `fsaverage6` atlases use the packaged white mesh and need no
  TemplateFlow download.

- `plot_brain(panel_layout = "presentation")` now uses the full cortex
  silhouette for panel placement, scale, and framing. Sparse atlases
  such as Wang retain their anatomical position and size, with
  consistent framing whether or not `background = TRUE` is used.

- Added
  [`get_hcpex_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_hcpex_atlas.md)
  and `get_atlas("hcpex")` for HCPex v1.1: 360 cortical and 66
  subcortical regions at 1 or 2 mm in the source-declared
  MNI152NLin2009cAsym space. Downloads use a pinned upstream revision
  and checksum-verified caching. Native IDs, abbreviations, full names,
  colors, hemisphere, and cortical/subcortical membership are available
  for ROI work; metadata includes citations, license, file receipts, and
  resampling history.

- Added a shared, versioned metadata record for atlases and loaded
  templates:
  [`atlas_metadata()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_metadata.md),
  [`template_metadata()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_metadata.md),
  [`atlas_citations()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_citations.md),
  and
  [`template_citations()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_citations.md).
  Records contain resource identity, actual geometry, role-specific
  references, file receipts, and structured processing history. Citation
  access and inspection work offline and survive R serialization.

- Atlas summaries now consistently display spatial metadata and citation
  information. Resampled atlases report their actual voxel spacing;
  original source resolutions remain in artifact records. Grid
  resampling retains the native anatomical template identity and records
  the requested grid separately.

- Subsetting and dilation preserve metadata and record their parameters.
  Merges retain both parent records and citations and reject conflicting
  grids or known template identities. Probability-volume collections and
  surface template pairs retain their component records.

- Missing provenance defaults to uncertain. The bundled ASEG atlas now
  reports `MNI152_unspecified`: header geometry does not verify the
  precise anatomical template. Olsen’s original atlas publication
  remains explicitly unverified. See the new “Identify, cite, and trace
  an atlas” vignette.

- [`dilate_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/dilate_atlas.md)
  now genuinely honours its `radius` argument. The previous
  implementation passed a fixed `k` to
  `Rnanoflann::nn(search = "radius")`, which returns the `k` nearest
  neighbours regardless of distance, so `radius` had no effect and
  dilation filled the entire mask (absorbing, for a cortical atlas,
  distant cerebellar and deep subcortical grey matter). Dilation now
  uses a standard k-NN search with an explicit Euclidean radius cutoff:
  in-mask voxels with no parcel within `radius` voxels are left
  unassigned. This is a behaviour change — callers that relied on the
  old whole-mask fill will now get radius-limited results. See the new
  “Dilating an Atlas to Cover Grey Matter” vignette.

- Added
  [`get_harvard_oxford_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_harvard_oxford_atlas.md)
  and registry entries for Harvard-Oxford cortical, subcortical, and
  combined structural parcellations. The default source is TemplateFlow,
  with threshold and resolution options for maximum-probability `dseg`
  images.

- Added
  [`get_fsl_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_fsl_atlas.md)
  for FSL XML-described atlases, including the documented offset between
  probabilistic XML label indices and max-probability summary image
  label values. Added a thin FSL-backed wrapper for Julich-Brain /
  Brodmann-style cytoarchitectonic labels
  ([`get_julich_brain_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_julich_brain_atlas.md)),
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

- Added a canonical
  [`new_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_constructor.md)
  /
  [`new_surfatlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_constructor.md)
  constructor that assembles every loader’s return value (Schaefer,
  Glasser, ASEG, Olsen MTL / hippocampus, TemplateFlow subcortical). The
  constructor validates required fields with a typed
  `neuroatlas_error_invalid_atlas` condition, normalises RGB colour maps
  to a data frame, builds `roi_metadata` uniformly, and attaches
  `atlas_ref` / provenance in one place — removing ~100 lines of
  per-loader boilerplate.

- Added a lightweight atlas registry
  ([`register_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/register_atlas.md))
  exposed via two new public helpers:
  [`list_atlases()`](https://bbuchsbaum.github.io/neuroatlas/reference/list_atlases.md)
  enumerates the built-in atlases, and `get_atlas(name, ...)` dispatches
  to the registered loader by id or alias
  (e.g. `get_atlas("schaefer2018", parcels="100", networks="7")`).

- Added centralised download helpers
  ([`.neuroatlas_download()`](https://bbuchsbaum.github.io/neuroatlas/reference/dot-neuroatlas_download.md),
  [`.neuroatlas_try_download()`](https://bbuchsbaum.github.io/neuroatlas/reference/dot-neuroatlas_try_download.md))
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
  [`new_atlas_ref()`](https://bbuchsbaum.github.io/neuroatlas/reference/new_atlas_ref.md),
  [`atlas_ref()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_ref.md),
  [`atlas_family()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_family.md),
  [`atlas_space()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_space.md),
  [`atlas_coord_space()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_coord_space.md),
  and
  [`validate_atlas_ref()`](https://bbuchsbaum.github.io/neuroatlas/reference/validate_atlas_ref.md).

- Atlas constructors now attach structured provenance/space metadata and
  compatibility aliases (`space`, `template_space`, `coord_space`,
  `confidence`) for Schaefer, Glasser, ASEG, Olsen MTL/hippocampus, and
  TemplateFlow subcortical atlases.

- [`get_glasser_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_glasser_atlas.md)
  now accepts a `source` argument and defaults to `source = "mni2009c"`
  with fallback to legacy `xcpengine` when unavailable. Fallback paths
  are tagged with `confidence = "uncertain"`.

- Added `test-atlas-ref.R` coverage for atlas reference metadata and
  basic cross-representation label concordance checks.

- Added space-level transform planning utilities backed by
  `inst/extdata/transform_registry.csv`:
  [`space_transform_manifest()`](https://bbuchsbaum.github.io/neuroatlas/reference/space_transform_manifest.md),
  [`atlas_transform_plan()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_transform_plan.md),
  and scope-aware
  [`atlas_transform_manifest()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_transform_manifest.md).

- [`atlas_alignment()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_alignment.md)
  now consults the space transform registry for same-representation
  cross-template routes (e.g., NLin6Asym to 2009cAsym) and reports
  route-specific status/confidence.

- Fixed white gaps (“shards”) in
  [`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
  surface rendering caused by inconsistent triangle winding in some
  meshes.

- Added `silhouette*` and `network_border*` options to
  [`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
  for improved boundary styling (silhouette outline and between-network
  borders).

- Improved
  [`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
  aesthetics with smoother boundary rendering (`border_geom = "path"`)
  and an optional normal-based shading overlay (`shading*`,
  `fill_alpha`).

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
