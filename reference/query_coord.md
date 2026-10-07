# Query Atlas Labels by Coordinate

Atlas-first convenience wrappers for label lookup. \`query_coord()\`
queries world/mm coordinates, while \`query_vox()\` queries R-style
1-based voxel grid indices and converts them to world coordinates before
dispatching to \`query_point()\`.

## Usage

``` r
query_coord(x, coords, ...)

query_vox(x, ijk, ...)
```

## Arguments

- x:

  A single atlas object or a named list of atlas objects.

- coords:

  Numeric vector of length 3 or an N x 3 matrix of world/mm coordinates.

- ...:

  Additional arguments passed to \`query_point()\`, such as \`radius\`,
  \`from_space\`, or \`nearest\`.

- ijk:

  Numeric/integer vector of length 3 or an N x 3 matrix of R-style
  1-based voxel grid indices.

## Value

A tibble with atlas labels at the requested locations.

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
query_coord(atlas, matrix(c(1, 0, 0), nrow = 1), radius = 0)
#> # A tibble: 1 × 10
#>   point     x     y     z atlas_name    id label hemi  network id_convention
#>   <int> <dbl> <dbl> <dbl> <chr>      <int> <chr> <chr> <chr>   <chr>        
#> 1     1     1     0     0 toy            2 B     right NA      NA           
```
