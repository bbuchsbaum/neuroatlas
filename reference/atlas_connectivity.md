# Compute Connectivity Matrix from Atlas Parcellations

Extracts mean time-series for each atlas region from a 4D NeuroVec and
computes pairwise correlations to produce a connectivity matrix.

## Usage

``` r
atlas_connectivity(
  data_vol,
  atlas,
  method = c("pearson", "spearman", "partial"),
  threshold = NULL,
  stat_func = mean,
  ...
)
```

## Arguments

- data_vol:

  A `NeuroVec` (4D) containing the time-series data.

- atlas:

  An atlas object defining the parcellation.

- method:

  Character string specifying the correlation method: `"pearson"`
  (default), `"spearman"`, or `"partial"`. Partial correlation requires
  the corpcor package.

- threshold:

  Optional numeric value. If supplied, entries with `abs(r) < threshold`
  are set to zero.

- stat_func:

  Function used to summarise voxel values within each parcel (default:
  `mean`).

- ...:

  Additional arguments passed to [`reduce_atlas`](reduce_atlas.md).

## Value

A symmetric matrix of class `c("atlas_connectivity", "matrix")` with
region labels as dimnames.

## See also

[`reduce_atlas`](reduce_atlas.md) for the underlying extraction,
[`as_igraph.atlas_connectivity`](as_igraph.md) for graph conversion

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_schaefer_atlas(parcels = "100", networks = "7")
# data_vol is a 4D NeuroVec from an fMRI run
conn <- atlas_connectivity(data_vol, atlas)
conn_sparse <- atlas_connectivity(data_vol, atlas, threshold = 0.3)
} # }
```
