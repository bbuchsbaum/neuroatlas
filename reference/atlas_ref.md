# Atlas Reference Accessor

Returns the canonical atlas reference metadata for an atlas object.

## Usage

``` r
atlas_ref(x, ...)

# S3 method for class 'atlas'
atlas_ref(x, ...)

# Default S3 method
atlas_ref(x, ...)
```

## Arguments

- x:

  An atlas object.

- ...:

  Additional arguments passed to methods.

## Value

An object of class \`"atlas_ref"\`.

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
atlas_ref(atlas)
```
