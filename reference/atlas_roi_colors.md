# Assign optimal colours to atlas regions

Bridge between an atlas object and the `roi_colors_*()` family of colour
algorithms. Extracts ROI centroids, assembles a metadata tibble, and
dispatches to the requested algorithm.

## Usage

``` r
atlas_roi_colors(atlas, method = "rule_hcl", ...)
```

## Arguments

- atlas:

  An S3 atlas object (any type: Schaefer, ASEG, Olsen, etc.).

- method:

  One of `"rule_hcl"` (default), `"network_harmony"`, `"maximin_view"`,
  `"embedding"`, or a named character vector of hex colours keyed by
  region ID.

- ...:

  Additional arguments passed to the underlying `roi_colors_*()`
  function.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
columns `id` (integer) and `color` (hex string), one row per region in
the atlas.

## Details

When `method` is a character vector of hex colours (named by region ID
or in the same order as `atlas$ids`), no colour algorithm is invoked and
the colours are returned directly.

## Examples

``` r
# \donttest{
atlas <- get_aseg_atlas()
cols <- atlas_roi_colors(atlas)
head(cols)
#> # A tibble: 6 × 2
#>      id color  
#>   <dbl> <chr>  
#> 1    10 #FC90AD
#> 2    11 #EAA06D
#> 3    12 #F19B7F
#> 4    13 #F99596
#> 5    16 #EB7DAD
#> 6    17 #FD8EB8
# }
```
