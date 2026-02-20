# Transform Coordinates Between Spaces

Apply an affine transformation to convert 3D coordinates from one
neuroimaging coordinate space to another.

## Usage

``` r
transform_coords(
  coords,
  from = NULL,
  to = NULL,
  transform = NULL,
  coords_as_cols = FALSE
)
```

## Arguments

- coords:

  Numeric matrix of 3D coordinates to transform. Can be:

  - An N x 3 matrix (N points, each row is x/y/z)

  - A 3 x N matrix (if `coords_as_cols = TRUE`)

  - A length-3 vector (single point)

- from:

  Character string specifying the source coordinate space. One of
  "MNI305", "MNI152".

- to:

  Character string specifying the target coordinate space. One of
  "MNI305", "MNI152".

- transform:

  Optional 4x4 affine matrix. If provided, `from` and `to` are ignored
  and this matrix is applied directly.

- coords_as_cols:

  Logical. If `TRUE`, coordinates are in columns (3 x N matrix). If
  `FALSE` (default), coordinates are in rows (N x 3 matrix).

## Value

Transformed coordinates in the same format as the input.

## Details

The transformation is applied as: `p_new = M %*% [p; 1]` where M is the
4x4 affine matrix and p is each 3D point in homogeneous coordinates.

## See also

[`get_space_transform`](get_space_transform.md),
[`MNI305_to_MNI152`](MNI305_to_MNI152.md)

## Examples

``` r
# Transform a single point
p305 <- c(10, -20, 35)
p152 <- transform_coords(p305, from = "MNI305", to = "MNI152")
print(p152)
#> [1]  10.6941 -18.4064  36.1385

# Transform multiple points (N x 3 matrix)
points_305 <- rbind(
  c(10, -20, 35),
  c(0, 0, 0),
  c(-30, 15, 50)
)
points_152 <- transform_coords(points_305, from = "MNI305", to = "MNI152")
print(points_152)
#>          [,1]     [,2]    [,3]
#> [1,]  10.6941 -18.4064 36.1385
#> [2,]  -0.0429   1.5496  1.1840
#> [3,] -29.1974  16.0051 51.2895

# Round-trip: MNI305 -> MNI152 -> MNI305
p_roundtrip <- transform_coords(
  transform_coords(p305, "MNI305", "MNI152"),
  "MNI152", "MNI305"
)
all.equal(p305, p_roundtrip, tolerance = 1e-10)
#> [1] TRUE
```
