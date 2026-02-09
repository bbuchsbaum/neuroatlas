# Get Transform Matrix Between Coordinate Spaces

Retrieve the affine transformation matrix for converting coordinates
between two standard neuroimaging coordinate spaces.

## Usage

``` r
get_space_transform(from, to)
```

## Arguments

- from:

  Character string specifying the source coordinate space. One of
  "MNI305", "MNI152".

- to:

  Character string specifying the target coordinate space. One of
  "MNI305", "MNI152".

## Value

A 4x4 numeric matrix representing the affine transform. Returns the
identity matrix if `from == to`. Returns `NULL` with a warning if the
transform is not available.

## See also

[`transform_coords`](transform_coords.md),
[`MNI305_to_MNI152`](MNI305_to_MNI152.md)

## Examples

``` r
# Get the MNI305 to MNI152 transform
xfm <- get_space_transform("MNI305", "MNI152")

# Identity for same-space
id <- get_space_transform("MNI152", "MNI152")
all.equal(id, diag(4))
#> [1] TRUE
```
