# Check if a Nonlinear Template Warp is Needed

Determines whether two template spaces are in the same coordinate system
but on different template grids, requiring a nonlinear warp.

## Usage

``` r
needs_template_warp(from, to)
```

## Arguments

- from:

  Source template space identifier.

- to:

  Target template space identifier.

## Value

Logical. \`TRUE\` if the templates share a coordinate space but differ
in grid/registration, \`FALSE\` otherwise, \`NA\` if either space is
unknown.

## Details

This function identifies cases where an affine transform is NOT needed
but a nonlinear warp IS needed. For example, MNI152NLin6Asym and
MNI152NLin2009cAsym are both in MNI152 coordinate space but use
different nonlinear registration targets, so voxel grids don't align
exactly (~2mm difference).

## See also

\[needs_coord_transform()\] for checking coordinate-space differences,
\[atlas_transform_plan()\] for planning multi-step transforms.

## Examples

``` r
needs_template_warp("MNI152NLin6Asym", "MNI152NLin2009cAsym")  # TRUE
#> [1] TRUE
needs_template_warp("fsaverage", "fsaverage6")                   # TRUE
#> [1] TRUE
needs_template_warp("fsaverage", "MNI152")                       # FALSE (different coord spaces)
#> [1] FALSE
needs_template_warp("MNI152NLin6Asym", "MNI152NLin6Asym")       # FALSE (identical)
#> [1] FALSE
```
