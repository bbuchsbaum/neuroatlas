# MNI152 to MNI305 Affine Transform Matrix

The inverse of [`MNI305_to_MNI152`](MNI305_to_MNI152.md), for converting
RAS coordinates from MNI152 space back to MNI305 (fsaverage) space.

## Usage

``` r
MNI152_to_MNI305
```

## Format

A 4x4 numeric matrix (affine transform in homogeneous coordinates).

## Details

This matrix is derived from FreeSurfer's `mni152.register.dat` file,
located at `$FREESURFER_HOME/average/mni152.register.dat`.

The transform accounts for the approximately 4mm difference between
MNI305 and MNI152 coordinate systems. It includes small rotation,
scaling, and translation components.

To apply: for a point `p = c(R, A, S)`, compute
`MNI305_to_MNI152 %*% c(p, 1)` and take the first 3 elements.

## See also

[`MNI305_to_MNI152`](MNI305_to_MNI152.md),
[`transform_coords`](transform_coords.md)

## Examples

``` r
# Transform a point from MNI152 to MNI305
point_152 <- c(10.695, -18.409, 36.137)
point_305 <- (MNI152_to_MNI305 %*% c(point_152, 1))[1:3]
print(point_305)  # approximately c(10, -20, 35)
#> [1]  10.00091 -20.00261  34.99848
```
