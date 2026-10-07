# Get Atlas Processing History

Get Atlas Processing History

## Usage

``` r
atlas_history(x, ...)
```

## Arguments

- x:

  An atlas object.

- ...:

  Additional arguments passed to methods.

## Value

A tibble with one row per processing step tracked by `neuroatlas`.

## Examples

``` r
grid <- neuroim2::NeuroSpace(c(2, 1, 1))
volume <- neuroim2::NeuroVol(array(c(1, 2), c(2, 1, 1)), grid)
atlas <- structure(
  list(
    atlas = volume, ids = 1:2,
    labels = c("A", "B"), orig_labels = c("A", "B"),
    hemi = c("left", "right"), name = "toy",
    cmap = rbind(c(1, 0, 0), c(0, 0, 1)),
    atlas_ref = new_atlas_ref("toy", "two-regions",
      template_space = "MNI152NLin6Asym", coord_space = "MNI152"
    )
  ),
  class = "atlas"
)
atlas_history(atlas)
#> # A tibble: 0 × 12
#> # ℹ 12 variables: step <int>, action <chr>, representation <chr>,
#> #   from_template_space <chr>, to_template_space <chr>, from_coord_space <chr>,
#> #   to_coord_space <chr>, status <chr>, confidence <chr>, details <chr>,
#> #   parameters <list>, software_version <chr>
```
