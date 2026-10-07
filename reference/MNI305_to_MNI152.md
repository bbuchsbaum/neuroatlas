# MNI305 to MNI152 Affine Transform Matrix

A legacy 4x4 FreeSurfer affine for approximate coordinate conversion
from MNI305 to the broad MNI152 family. It does not encode an exact MNI
variant.

## Usage

``` r
MNI305_to_MNI152
```

## Format

A 4x4 numeric matrix (affine transform in homogeneous coordinates).

## Source

FreeSurfer: `$FREESURFER_HOME/average/mni152.register.dat`

## Details

This matrix is derived from FreeSurfer's `mni152.register.dat` file,
located at `$FREESURFER_HOME/average/mni152.register.dat`.

It includes rotation, scaling and translation components. Anatomical
error depends on position and the source/target registrations; it is not
a uniform displacement or a qualified volume-to-cortex mapping.

To apply: for a point `p = c(R, A, S)`, compute
`MNI305_to_MNI152 %*% c(p, 1)` and take the first 3 elements.

## See also

<https://surfer.nmr.mgh.harvard.edu/fswiki/CoordinateSystems>
[`MNI152_to_MNI305`](https://bbuchsbaum.github.io/neuroatlas/reference/MNI152_to_MNI305.md),
[`transform_coords`](https://bbuchsbaum.github.io/neuroatlas/reference/transform_coords.md)

## Examples

``` r
# Transform a single point from MNI305 to MNI152
point_305 <- c(10, -20, 35)
point_152 <- (MNI305_to_MNI152 %*% c(point_305, 1))[1:3]
print(point_152) # approximately c(10.695, -18.409, 36.137)
#> [1]  10.6941 -18.4064  36.1385
```
