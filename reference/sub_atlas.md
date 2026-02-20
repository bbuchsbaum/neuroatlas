# Select a Subset of Atlas Regions

Return a new atlas containing only the requested regions. Regions can be
identified by integer ID (matching `x$ids`), by label (matching
`x$labels`), or by hemisphere. When multiple selection criteria are
given they are intersected.

## Usage

``` r
sub_atlas(x, ids = NULL, labels = NULL, hemi = NULL, ...)

# S3 method for class 'atlas'
sub_atlas(x, ids = NULL, labels = NULL, hemi = NULL, ...)
```

## Arguments

- x:

  An atlas object.

- ids:

  Integer or numeric vector of region IDs to retain (matched against
  `x$ids`).

- labels:

  Character vector of region labels to retain (matched against
  `x$labels`).

- hemi:

  Character vector of hemispheres to retain (`"left"` and/or `"right"`).

- ...:

  Additional arguments passed to methods.

## Value

An atlas object of the same class as `x` containing only the selected
regions. Voxels (or vertices) not belonging to the selected regions are
excluded.

## Details

This is the atlas-level analogue of
[`sub_clusters`](https://bbuchsbaum.github.io/neuroim2/reference/sub_clusters.html)
for
[`ClusteredNeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/ClusteredNeuroVol-class.html)
objects.

## See also

[`filter_atlas`](filter_atlas.md) for tidy-eval filtering by metadata
columns, [`get_roi`](get_roi.md) for extracting ROI volumes

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_aseg_atlas()

# By ID
sub <- sub_atlas(atlas, ids = c(10, 11, 12))

# By label
sub2 <- sub_atlas(atlas, labels = c("Thalamus", "Caudate"))

# By hemisphere
left <- sub_atlas(atlas, hemi = "left")

# Combined: left-hemisphere regions matching specific labels
sub3 <- sub_atlas(atlas, labels = c("Thalamus", "Caudate"), hemi = "left")
} # }
```
