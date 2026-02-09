# Reduce a NeuroVol or NeuroVec by an Atlas

Applies a summary function to data within each ROI defined by an atlas.
This is an S3 generic function.

## Usage

``` r
reduce_atlas(atlas, data_vol, stat_func, ..., format = NULL)

# S3 method for class 'atlas'
reduce_atlas(atlas, data_vol, stat_func, ..., format = NULL)
```

## Arguments

- atlas:

  An atlas object or another object for which a method is defined.

- data_vol:

  A `NeuroVol` (3D) or `NeuroVec` (4D) with the data to be summarized.
  When a 3D volume is supplied the result contains a single row; 4D
  inputs yield one row per time point.

- stat_func:

  The function to apply to the data within each ROI (e.g., `mean`, `sd`,
  `sum`).

- ...:

  Additional arguments passed to `stat_func`.

- format:

  Character string specifying output format: "wide" or "long". If NULL
  (default), uses "long" for NeuroVol and "wide" for NeuroVec.

## Value

A `tibble` with format depending on the `format` parameter:

- `"wide"`: Regions as columns. For NeuroVec, rows are time points.

- `"long"`: Columns are `region` and `value` (NeuroVol) or `time`,
  `region`, and `value` (NeuroVec).

## Details

When `data_vol` is a 3D `NeuroVol`, the returned tibble contains a
single row with one column per ROI. If a 4D `NeuroVec` is supplied, each
time point is summarised separately and a `time` column is added to the
tibble.

## See also

[`map_atlas`](map_atlas.md) for mapping values to atlas regions,
[`get_roi`](get_roi.md) for extracting specific regions

## Examples

``` r
if (FALSE) { # \dontrun{
# Load an atlas
atlas <- get_schaefer_atlas(parcels = "200", networks = "7")

# Create example data (random values in brain space)
brain_data <- neuroim2::NeuroVol(rnorm(prod(dim(atlas$atlas))),
                                 space = space(atlas$atlas))

# Compute mean values within each atlas region
region_means <- reduce_atlas(atlas, brain_data, mean)

# Compute standard deviation within each region
region_sds <- reduce_atlas(atlas, brain_data, sd, na.rm = TRUE)
} # }
```
