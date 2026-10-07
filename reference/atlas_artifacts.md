# Get Atlas Artifact Metadata

Get Atlas Artifact Metadata

## Usage

``` r
atlas_artifacts(x, ...)
```

## Arguments

- x:

  An atlas object.

- ...:

  Additional arguments passed to methods.

## Value

A tibble with one row per upstream artifact used to construct the atlas.

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
atlas_artifacts(atlas)
#> # A tibble: 0 × 27
#> # ℹ 27 variables: role <chr>, family <chr>, model <chr>, variant <chr>,
#> #   source_name <chr>, source_url <chr>, source_ref <chr>,
#> #   source_version <chr>, citation_doi <chr>, license <chr>, license_url <chr>,
#> #   file_name <chr>, local_path <chr>, sha256 <chr>, checksum <chr>,
#> #   checksum_algorithm <chr>, checksum_basis <chr>, template_space <chr>,
#> #   coord_space <chr>, resolution <chr>, density <chr>, parcels <chr>,
#> #   networks <chr>, hemi <chr>, lineage <chr>, confidence <chr>, notes <chr>
```
