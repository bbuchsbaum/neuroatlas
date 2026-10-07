# Extract Parcel Values Aligned to an Atlas

Returns a vector aligned to \`atlas\$ids\`, suitable for \`map_atlas()\`
or \`plot_brain()\`.

## Usage

``` r
parcel_values(x, atlas, column = "value")
```

## Arguments

- x:

  A \`parcel_data\` object.

- atlas:

  An atlas object.

- column:

  Value column in \`x\$parcels\` to extract.

## Value

A vector with \`length(atlas\$ids)\` elements ordered to \`atlas\$ids\`.

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
x <- parcel_data(data.frame(
  id = 1:2, label = c("A", "B"),
  hemi = c("left", "right"), value = c(0.2, 0.7)
), atlas_id = "toy")
parcel_values(x, atlas)
#> [1] 0.2 0.7
```
