# Map Values to Schaefer Atlas Format

\`r lifecycle::badge("deprecated")\`

This function has been deprecated. Use [`map_atlas()`](map_atlas.md)
directly or [`plot_brain()`](plot_brain.md) for visualisation.

## Usage

``` r
map_to_schaefer(atlas, vals, thresh = NULL, pos = FALSE)
```

## Arguments

- atlas:

  An atlas object containing Schaefer parcellation information

- vals:

  Numeric vector of values to map to atlas regions

- thresh:

  Numeric vector of length 2 specifying (min, max) thresholds.

- pos:

  Logical. If TRUE, uses raw values; if FALSE, uses absolute values for
  thresholding. Default: FALSE

## Value

A tibble with mapped values.

## Examples

``` r
# \donttest{
# Deprecated — use map_atlas() or plot_brain() instead
# }
```
