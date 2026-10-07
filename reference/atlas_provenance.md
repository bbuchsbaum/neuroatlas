# Atlas Provenance Accessors

Access structured provenance metadata for atlas objects, including the
canonical atlas identity, upstream artifacts, and processing history.

## Usage

``` r
atlas_provenance(x, ...)

# S3 method for class 'atlas'
atlas_provenance(x, ...)

# Default S3 method
atlas_provenance(x, ...)

# S3 method for class 'atlas'
atlas_artifacts(x, ...)

# Default S3 method
atlas_artifacts(x, ...)

# S3 method for class 'atlas'
atlas_history(x, ...)

# Default S3 method
atlas_history(x, ...)
```

## Arguments

- x:

  An atlas object.

- ...:

  Additional arguments passed to methods.

## Value

A list of class \`"atlas_provenance"\` with fields:

- ref:

  Canonical
  [`atlas_ref()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_ref.md)
  identity metadata.

- artifacts:

  A tibble describing upstream files/resources.

- history:

  A tibble describing processing steps applied in `neuroatlas`.

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
atlas_provenance(atlas)
#> <atlas_provenance>
#>   family: toy 
#>   model: two-regions 
#>   artifacts: 0 
#>   history steps: 0 
```
