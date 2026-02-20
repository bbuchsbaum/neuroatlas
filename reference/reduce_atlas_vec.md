# Reduce a NeuroVec by an Atlas to a ClusteredNeuroVec

Averages (or otherwise summarises) a 4D image within each atlas parcel,
returning a
[`ClusteredNeuroVec`](https://bbuchsbaum.github.io/neuroim2/reference/ClusteredNeuroVec.html)
whose voxels share one time-series per region.

## Usage

``` r
reduce_atlas_vec(atlas, data_vol, mask, ...)

# S3 method for class 'atlas'
reduce_atlas_vec(
  atlas,
  data_vol,
  mask,
  stat_func = mean,
  dilate = FALSE,
  radius = 4,
  maxn = 50,
  ...
)
```

## Arguments

- atlas:

  An atlas object.

- data_vol:

  A `NeuroVec` (4D) with the data to be summarised.

- mask:

  A `NeuroVol` or `LogicalNeuroVol` brain mask. Non-zero values are
  treated as TRUE.

- ...:

  Additional arguments passed to methods.

- stat_func:

  Function used to aggregate voxel values within each parcel (default:
  `mean`).

- dilate:

  Logical. If `TRUE`, atlas parcels are dilated into the mask before
  averaging (via [`dilate_atlas`](dilate_atlas.md)).

- radius:

  Numeric dilation radius in voxels (passed to
  [`dilate_atlas`](dilate_atlas.md)). Default: 4.

- maxn:

  Integer maximum neighbours for dilation (passed to
  [`dilate_atlas`](dilate_atlas.md)). Default: 50.

## Value

A
[`ClusteredNeuroVec`](https://bbuchsbaum.github.io/neuroim2/reference/ClusteredNeuroVec.html).

## Details

The atlas volume is intersected with the brain mask so that only voxels
inside the mask contribute to each parcel average.

Cluster IDs are remapped to contiguous `1:K` before constructing the
`ClusteredNeuroVec`. This is required because the `[,,,t]` accessor in
neuroim2 uses cluster IDs as direct column indices into the internal
time-series matrix.

Parcels that have no voxels inside the mask receive `NA` time-series in
the output.

## See also

[`reduce_atlas`](reduce_atlas.md) for a tibble-based summary,
[`dilate_atlas`](dilate_atlas.md) for expanding parcels into a mask.

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
# data_vol: a NeuroVec with matching spatial dimensions
# mask:     a brain mask NeuroVol
cvec <- reduce_atlas_vec(atlas, data_vol, mask)
ts_mat <- as.matrix(cvec)   # T x K cluster time-series
} # }
```
