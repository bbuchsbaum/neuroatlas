# Filter Atlas by ROI Attributes

Subsets an atlas object to include only ROIs matching specified
criteria. Uses non-standard evaluation (NSE) similar to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

## Usage

``` r
filter_atlas(x, ..., .dots = NULL)

# S3 method for class 'atlas'
filter_atlas(x, ..., .dots = NULL)
```

## Arguments

- x:

  An atlas object

- ...:

  Filter expressions using column names from `roi_metadata(x)`. Multiple
  conditions are combined with AND.

- .dots:

  A list of quosures for standard evaluation (advanced use)

## Value

A new atlas object of the same class containing only the matching ROIs.
The returned atlas has updated `ids`, `labels`, `hemi`, `orig_labels`,
`cmap`, `network` (if present), and `roi_metadata` fields. The
underlying volume/surface data is also subset.

## Details

The filter operation creates a new atlas containing only the specified
ROIs. For volume atlases, voxels belonging to excluded ROIs are set to
zero. ROI IDs are preserved (not renumbered) to maintain consistency
with the original atlas labeling.

## See also

[`roi_metadata`](roi_metadata.md) for viewing available filter columns,
[`roi_attributes`](roi_attributes.md) for listing available attributes,
[`get_roi`](get_roi.md) for extracting ROI data

## Examples

``` r
if (FALSE) { # \dontrun{
# Filter Schaefer atlas to left hemisphere visual network
atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
left_vis <- filter_atlas(atlas, hemi == "left", network == "Vis")

# Filter ASEG to left hemisphere structures
aseg <- get_aseg_atlas()
left_aseg <- filter_atlas(aseg, hemi == "left")

# Filter by label pattern
hippo <- filter_atlas(aseg, grepl("Hippocampus", label))
} # }
```
