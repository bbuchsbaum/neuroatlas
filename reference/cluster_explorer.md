# Launch Cluster Explorer Shiny App

Interactive explorer linking a cluster summary table, parcel brain map,
and design-aware signal plots. Clusters are formed volumetrically from
`stat_map`; visualization is parcel-level on `surfatlas`.

## Usage

``` r
cluster_explorer(
  data_source = NULL,
  atlas = NULL,
  stat_map = NULL,
  surfatlas = NULL,
  sample_table = NULL,
  design = NULL,
  threshold = 3,
  min_cluster_size = 20,
  connectivity = c("26-connect", "18-connect", "6-connect"),
  tail = c("two_sided", "positive", "negative"),
  series_fun = NULL,
  overlay_space = NULL,
  overlay_density = NULL,
  overlay_resolution = NULL,
  overlay_fun = c("avg", "nn", "mode"),
  overlay_sampling = c("midpoint", "normal_line", "thickness"),
  prefetch = TRUE,
  prefetch_max_clusters = 200,
  prefetch_max_voxels = 1e+05,
  palette = "vik",
  selection_engine = c("cluster", "parcel", "sphere", "custom"),
  parcel_ids = NULL,
  sphere_centers = NULL,
  sphere_radius = 6,
  sphere_units = c("mm", "voxels"),
  sphere_combine = c("separate", "union"),
  selection_provider = NULL,
  analysis_plugins = NULL,
  default_analysis_plugin = "none"
)
```

## Arguments

- data_source:

  A sample-wise data source supporting
  `neuroim2::series(data_source, i)` where `i` is voxel coordinates
  (`n x 3` matrix) or indices. Rows of the returned matrix correspond to
  samples/design rows.

- atlas:

  A volumetric `atlas` object used for parcel annotation. When atlas and
  `stat_map` dimensions differ, the atlas is automatically resampled to
  `stat_map` space (nearest-neighbor labels) before cluster annotation.

- stat_map:

  A `NeuroVol` statistic image used for thresholding and clustering.

- surfatlas:

  A surface atlas object used by [`plot_brain()`](plot_brain.md). If
  `NULL`, the function attempts to infer a surface atlas from a
  compatible \`atlas\` input (for example Schaefer or Glasser). If
  inference is not possible, and non-demo inputs are otherwise present,
  input validation fails.

- sample_table:

  Optional data frame with one row per sample. If `NULL`, a default
  table with `.sample_index` is created.

- design:

  Optional design table (one row per sample). When provided, it is
  column-bound to `sample_table`.

- threshold:

  Numeric threshold used for cluster formation.

- min_cluster_size:

  Minimum number of voxels required to keep a cluster.

- connectivity:

  Connectivity passed to
  [`neuroim2::conn_comp()`](https://bbuchsbaum.github.io/neuroim2/reference/conn_comp-methods.html).

- tail:

  Clustering mode: `"positive"`, `"negative"`, or `"two_sided"`.

- series_fun:

  Optional function override for extracting voxel-wise sample series.
  Must accept `(data_source, i)` and return a matrix-like object with
  one row per sample.

- overlay_space:

  Optional surface space override used to fetch white/pial meshes for
  volumetric cluster projection. Defaults to `surfatlas$surface_space`.

- overlay_density:

  Optional TemplateFlow density override for overlay surface loading.

- overlay_resolution:

  Optional TemplateFlow resolution override for overlay surface loading.

- overlay_fun:

  Reduction used by
  [`neurosurf::vol_to_surf()`](https://rdrr.io/pkg/neurosurf/man/vol_to_surf.html).

- overlay_sampling:

  Sampling strategy for
  [`neurosurf::vol_to_surf()`](https://rdrr.io/pkg/neurosurf/man/vol_to_surf.html).

- prefetch:

  Logical default for eager cluster signal prefetch.

- prefetch_max_clusters:

  Default max cluster count allowed for prefetch.

- prefetch_max_voxels:

  Default max total cluster voxels allowed for prefetch.

- palette:

  Continuous palette passed to [`plot_brain()`](plot_brain.md).

- selection_engine:

  Selection backend. `"cluster"` uses the built-in connected-component
  workflow. `"custom"` delegates data assembly to `selection_provider`.

- parcel_ids:

  Optional parcel ids used when `selection_engine = "parcel"`. Default
  `NULL` uses all parcels.

- sphere_centers:

  Optional sphere centers used when `selection_engine = "sphere"`.
  Provide numeric vector length 3 or matrix/data.frame with 3 columns.

- sphere_radius:

  Sphere radius used when `selection_engine = "sphere"`.

- sphere_units:

  Units for `sphere_centers` and `sphere_radius`: `"mm"` (world
  coordinates) or `"voxels"` (grid coordinates).

- sphere_combine:

  How to combine multiple spheres: `"separate"` (one region per sphere)
  or `"union"`.

- selection_provider:

  Optional function used when `selection_engine = "custom"`. Must return
  a list compatible with
  [`build_cluster_explorer_data()`](build_cluster_explorer_data.md)
  output.

- analysis_plugins:

  Optional list of analysis plugin definitions. Plugins can transform
  extracted time-series/design data before plotting.

- default_analysis_plugin:

  Optional default plugin id. Defaults to `"none"`.

## Value

A `shiny.appobj`.

## Details

Calling `cluster_explorer()` with no arguments launches a synthetic demo
dataset so the UI can be explored immediately.
