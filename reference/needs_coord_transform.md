# Check if a Coordinate Transform is Needed

Determines whether two template spaces require an affine coordinate
transform (e.g., MNI305 to MNI152).

## Usage

``` r
needs_coord_transform(from, to)
```

## Arguments

- from:

  Source template space identifier.

- to:

  Target template space identifier.

## Value

Logical. \`TRUE\` if the coordinate spaces differ, \`FALSE\` if they
match, \`NA\` if either space is unknown.

## Details

This checks whether the \*coordinate systems\* differ (MNI305 vs
MNI152), which requires an affine transform. It does NOT check whether
the template grids differ within the same coordinate system (use
\[needs_template_warp()\] for that).

## See also

\[needs_template_warp()\] for checking template-grid differences,
\[template_to_coord_space()\] for the underlying lookup.

## Examples

``` r
needs_coord_transform("fsaverage", "MNI152NLin6Asym")  # TRUE
#> [1] TRUE
needs_coord_transform("fsaverage", "MNI305")            # FALSE
#> [1] FALSE
needs_coord_transform("MNI152NLin6Asym", "MNI152NLin2009cAsym")  # FALSE
#> [1] FALSE
```
