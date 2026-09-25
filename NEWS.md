# neuroatlas 0.1.0.9006

* `plot(<atlas>, view = "ortho")` works again with neuroim2 >= 0.19, whose
  redesigned `plot_ortho()` returns one assembled figure by default. The
  per-plane panels are now requested with `assemble = FALSE`, drawn without
  raster interpolation (which blended neighbouring region colours) or head
  cropping, and region-ID mapping tolerates non-numeric slice values. Older
  neuroim2 releases are handled unchanged.

* `surface_anatomy()` gains `type = "sulcal_depth"`, a FreeSurfer-style
  sulcal-depth proxy from the white and displayed (inflated) surfaces. It gives
  the broad two-tone gyral/sulcal underlay used by Workbench and pycortex; the
  default (`"curvature"`) is unchanged, and explicit or atlas metrics still win.

# neuroatlas 0.1.0.9005

* The CPU parcel renderer again uses the sulcal-depth underlay on inflated
  fsaverage6 atlases (e.g. Schaefer): a declared atlas density no longer
  diverts the white-mesh lookup away from the packaged meshes, which had
  fallen back to raw curvature.
* Parcel-map colorbars label the limits, threshold, and zero without
  crowding, set the threshold ticks in bold, and are wider.

# neuroatlas 0.1.0.9004

* `plot_brain(static_backend = "cpu", vals = ...)` now renders publication
  parcel maps with `neurosurf::render_surface_parcels()`: smooth antialiased
  parcel edges, a sulcal-depth underlay with soft lighting, a flat medial wall
  drawn only in medial views, and a figure whose layout (2x2 grid or single
  row, colorbar below or beside) adapts to the device. New arguments
  `vals_threshold` (unfills sub-threshold parcels and greys that band of the
  colorbar) and `parcel_style`. The CPU backend previously rejected `vals`.

# neuroatlas 0.1.0.9003

* Enabled the qualified `MNI152NLin6Asym` / `MNI152NLin2009cAsym` ANTs
  transform pair in both directions, with published artifact hashes, input and
  software provenance, and qualification on 1 mm and 2 mm target grids.
* Added `get_template_transform()`, `apply_template_transform()`, and
  `transform_atlas()` for explicit template-space routes and target grids.
  Routes compose before a single resampling pass. Label IDs and metadata are
  retained, probability channels are never renormalized, and provenance records
  the artifact hashes, grids, and interpolation.
* Added verified transform caching with atomic downloads, offline operation,
  and cache cleanup restricted to neuroatlas-owned files. The route planner
  handles longer paths and can restrict resolution to executable releases.

# neuroatlas 0.1.0.9000

* New `surface_anatomy()` resolves the sulcal shading metric shared by the
  CPU surface renderers: mean curvature of the matching white mesh, used only
  when its topology matches the display mesh, with explicit provenance.
  Default `fsaverage6` atlases use the packaged white mesh and need no
  TemplateFlow download.

* `plot_brain(panel_layout = "presentation")` now uses the full cortex
  silhouette for panel placement, scale, and framing. Sparse atlases such as
  Wang retain their anatomical position and size, with consistent framing
  whether or not `background = TRUE` is used.

* Added `get_hcpex_atlas()` and `get_atlas("hcpex")` for HCPex v1.1:
  360 cortical and 66 subcortical regions at 1 or 2 mm in the source-declared
  MNI152NLin2009cAsym space. Downloads use a pinned upstream revision and
  checksum-verified caching. Native IDs, abbreviations, full names, colors,
  hemisphere, and cortical/subcortical membership are available for ROI work;
  metadata includes citations, license, file receipts, and resampling history.

* Added a shared, versioned metadata record for atlases and loaded templates:
  `atlas_metadata()`, `template_metadata()`, `atlas_citations()`, and
  `template_citations()`. Records contain resource identity, actual geometry,
  role-specific references, file receipts, and structured processing history.
  Citation access and inspection work offline and survive R serialization.
* Atlas summaries now consistently display spatial metadata and citation
  information. Resampled atlases report their actual voxel spacing; original
  source resolutions remain in artifact records. Grid resampling retains the
  native anatomical template identity and records the requested grid separately.
* Subsetting and dilation preserve metadata and record their parameters. Merges
  retain both parent records and citations and reject conflicting grids or
  known template identities. Probability-volume collections and surface
  template pairs retain their component records.
* Missing provenance defaults to uncertain. The bundled ASEG atlas now reports
  `MNI152_unspecified`: header geometry does not verify the precise anatomical
  template. Olsen's original atlas publication remains explicitly unverified.
  See the new "Identify, cite, and trace an atlas" vignette.

* `dilate_atlas()` now genuinely honours its `radius` argument. The
  previous implementation passed a fixed `k` to `Rnanoflann::nn(search =
  "radius")`, which returns the `k` nearest neighbours regardless of
  distance, so `radius` had no effect and dilation filled the entire
  mask (absorbing, for a cortical atlas, distant cerebellar and deep
  subcortical grey matter). Dilation now uses a standard k-NN search
  with an explicit Euclidean radius cutoff: in-mask voxels with no
  parcel within `radius` voxels are left unassigned. This is a
  behaviour change --- callers that relied on the old whole-mask fill
  will now get radius-limited results. See the new "Dilating an Atlas to
  Cover Grey Matter" vignette.
* Added `get_harvard_oxford_atlas()` and registry entries for
  Harvard-Oxford cortical, subcortical, and combined structural
  parcellations. The default source is TemplateFlow, with threshold and
  resolution options for maximum-probability `dseg` images.
* Added `get_fsl_atlas()` for FSL XML-described atlases, including the
  documented offset between probabilistic XML label indices and
  max-probability summary image label values. Added a thin FSL-backed
  wrapper for Julich-Brain / Brodmann-style cytoarchitectonic labels
  (`get_julich_brain_atlas()`), which now downloads the Nilearn/NITRC
  `Juelich.tgz` archive into a local FSL-style cache when `FSLDIR` is
  unset.
* `plot_brain(overlay = <NeuroVol>)` now propagates missing data
  through the volume-to-surface projection: vertices that fall outside
  the input volume's coverage (or whose neighbourhood contains no
  finite source voxel) are emitted as `NA` rather than `0`. Faces
  with no finite vertices are dropped from the polygon set, so
  uncovered cortex renders as transparent background instead of an
  opaque dark-palette wash. Faces with partial coverage continue to
  render using the average of their finite vertices. The internal
  `vol_to_surf()` `fill` argument changed from `0` to `NA_real_`.
* `plot_brain(overlay = <NeuroVol>)` now repairs legacy
  `SurfaceGeometry` objects on the fly. The bundled `data(fsaverage)`
  artefact and the `@geometry` slots inside packaged surface atlases
  were serialized before `neurosurf::SurfaceGeometry` gained the
  `label` and `surf_to_world` slots; accessing those slots on a
  legacy object errored out and caused `vol_to_surf()` to silently
  return all-NA overlays. `.resolve_overlay_surface_pair()` now
  rebuilds any geometry that fails `validObject()` via the current
  constructor before passing it to `vol_to_surf()`.
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
