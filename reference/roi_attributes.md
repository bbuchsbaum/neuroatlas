# List Available ROI Attributes

Returns the names of attributes available for ROIs in an atlas. This is
useful for discovering what metadata is available for filtering or
analysis.

## Usage

``` r
roi_attributes(x, ...)

# S3 method for class 'atlas'
roi_attributes(x, ...)
```

## Arguments

- x:

  An atlas object

- ...:

  Additional arguments passed to methods

## Value

A character vector of attribute names that contain meaningful (non-NA)
values. Excludes internal fields like color values and the id column.

## See also

[`roi_metadata`](roi_metadata.md) for getting the full metadata tibble

## Examples

``` r
if (FALSE) { # \dontrun{
# Discover available attributes
atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
roi_attributes(atlas)
#> [1] "label" "label_full" "hemi" "network"

# Compare with ASEG atlas (no network attribute)
aseg <- get_aseg_atlas()
roi_attributes(aseg)
#> [1] "label" "label_full" "hemi"
} # }
```
