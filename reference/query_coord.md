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

  Additional arguments passed to \`query_point()\`, such as \`radius\`
  or \`from_space\`.

- ijk:

  Numeric/integer vector of length 3 or an N x 3 matrix of R-style
  1-based voxel grid indices.

## Value

A tibble with atlas labels at the requested locations.
