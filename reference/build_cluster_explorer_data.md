# Build Cluster Explorer Data

Compute sign-aware volumetric connected components from a statistic map,
annotate them with atlas parcels, and extract cluster-level 4D signal
summaries aligned to sample rows.

## Usage

``` r
build_cluster_explorer_data(
  data_source,
  atlas,
  stat_map,
  sample_table = NULL,
  threshold = 3,
  min_cluster_size = 20,
  connectivity = c("26-connect", "18-connect", "6-connect"),
  tail = c("two_sided", "positive", "negative"),
  signal_fun = mean,
  signal_fun_args = list(na.rm = TRUE),
  series_fun = NULL,
  prefetch = TRUE,
  prefetch_max_clusters = Inf,
  prefetch_max_voxels = Inf,
  series_cache_env = NULL
)
```

## Arguments

- data_source:

  A sample-wise data source supporting
  `neuroim2::series(data_source, i)` where `i` is voxel coordinates
  (`n x 3` matrix) or indices. Rows of the returned matrix correspond to
  samples/design rows.

- atlas:

  A volumetric `atlas` object used for parcel annotation.

- stat_map:

  A `NeuroVol` statistic image used for thresholding and clustering.

- sample_table:

  Optional data frame with one row per sample. If `NULL`, a default
  table with `.sample_index` is created.

- threshold:

  Numeric threshold used for cluster formation.

- min_cluster_size:

  Minimum number of voxels required to keep a cluster.

- connectivity:

  Connectivity passed to
  [`neuroim2::conn_comp()`](https://bbuchsbaum.github.io/neuroim2/reference/conn_comp-methods.html).

- tail:

  Clustering mode: `"positive"`, `"negative"`, or `"two_sided"`.

- signal_fun:

  Function used to summarize cluster signal per sample.

- signal_fun_args:

  Named list of additional arguments passed to `signal_fun`.

- series_fun:

  Optional function override for extracting voxel-wise sample series.
  Must accept `(data_source, i)` and return a matrix-like object with
  one row per sample.

- prefetch:

  Logical; if `TRUE`, precompute signal summaries for all clusters on
  recompute.

- prefetch_max_clusters:

  Maximum clusters allowed for eager prefetch. Prefetch is skipped when
  exceeded.

- prefetch_max_voxels:

  Maximum total cluster voxels allowed for eager prefetch. Prefetch is
  skipped when exceeded.

- series_cache_env:

  Optional environment used to memoize voxel-coordinates-to-series
  extraction across cluster computations.

## Value

A list with:

- `cluster_table`:

  Cluster summary tibble.

- `cluster_parcels`:

  Cluster-parcel overlap tibble.

- `cluster_ts`:

  Sample-level cluster signal tibble.

- `cluster_voxels`:

  Named list of voxel coordinate matrices by cluster ID.

- `cluster_index`:

  A `NeuroVol` of global cluster IDs.

- `sample_table`:

  Normalized sample table with `.sample_index`.

- `prefetch_info`:

  List describing whether eager signal prefetch was requested/applied
  and the effective guard thresholds.
