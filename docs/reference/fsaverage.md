# Surface geometry for the fsaverage6 atlas

A list including left and right hemispheres for the orig, white,
inflated, and pial surfaces.

## Usage

``` r
data(fsaverage)
```

## Format

A named list with elements `lh` and `rh`, each containing surface
geometry objects for orig, white, inflated, and pial surfaces.

## Examples

``` r
# \donttest{
data(fsaverage)
names(fsaverage)
#> [1] "lh_inflated" "lh_orig"     "lh_pial"     "lh_white"    "rh_inflated"
#> [6] "rh_pial"     "rh_white"   
# }
```
