# Embedding-based palette with smooth gradients

Project ROI features to two dimensions (UMAP or PCA) and map polar angle
to hue, yielding palettes that look globally structured.

## Usage

``` r
roi_colors_embedding(
  rois,
  id_col = "roi",
  feature_cols = c("x", "y", "z"),
  hemi_col = NULL,
  method = c("umap", "pca"),
  C_range = c(40, 90),
  L = 65,
  seed = 1
)
```

## Arguments

- rois:

  Tibble with one ROI per row containing \`roi\`, \`x\`, \`y\`, and
  \`z\`.

- id_col:

  Column containing ROI IDs.

- feature_cols:

  Character vector of numeric or categorical columns that will be
  embedded (passed to \`stats::model.matrix()\`).

- hemi_col:

  Optional hemisphere column used for the final light/dark adjustment.

- method:

  Either \`"umap"\` (requires the \`uwot\` package) or \`"pca"\`.

- C_range:

  Chroma range mapped from radial distance in the embedding.

- L:

  Lightness value used for all ROIs before hemispheric adjustments.

- seed:

  Random seed for reproducibility.

## Value

Tibble with ROI IDs and colours.

## Examples

``` r
# \donttest{
rois <- data.frame(
  roi = 1:10, network = rep(c("Vis", "DMN"), 5),
  x = runif(10), y = runif(10), z = runif(10)
)
pal <- roi_colors_embedding(rois, feature_cols = c("x","y","z"),
                             method = "pca")
# }
```
