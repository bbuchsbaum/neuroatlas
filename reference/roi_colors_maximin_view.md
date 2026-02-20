# Slice-aware maximin palette for ROIs

Assign perceptually separated colours by penalising conflicts between
ROIs that are likely to appear together in axial/coronal/sagittal views.
This is a strong default for general slice and surface visualisation.

## Usage

``` r
roi_colors_maximin_view(
  rois,
  id_col = "roi",
  xyz_cols = c("x", "y", "z"),
  hemi_col = NULL,
  network_col = NULL,
  pair_col = NULL,
  k = 12,
  sigma_xy = 25,
  sigma_slice = 10,
  candidate_multiplier = 10,
  tau = 10,
  lambda_global = 0.15,
  bg_hex = "#808080",
  alpha = 0.85,
  seed = 1,
  weight_transform = NULL
)
```

## Arguments

- rois:

  Tibble with one ROI per row containing \`roi\`, \`x\`, \`y\`, and
  \`z\`.

- id_col:

  Column containing ROI IDs.

- xyz_cols:

  Character vector of length three giving the coordinates to use for
  distance calculations.

- hemi_col:

  Optional hemisphere column used for the final light/dark adjustment.

- network_col:

  Optional network label column to slightly boost cross-network
  conflicts.

- pair_col:

  Optional column identifying homologous L/R ROI pairs. When supplied,
  colours are first assigned to the homolog pair and then nudged per
  hemisphere.

- k:

  Number of nearest neighbours to evaluate when forming edges.

- sigma_xy:

  In-plane decay (mm) for slice visibility.

- sigma_slice:

  Through-slice decay (mm) for slice visibility.

- candidate_multiplier:

  How many candidate colours to generate relative to the number of ROIs.

- tau:

  Soft-min temperature; smaller emphasises hard minimum distance, larger
  smooths the objective.

- lambda_global:

  Optional additional term that encourages global colour diversity.

- bg_hex:

  Background grey (for contrast filtering).

- alpha:

  Opacity of overlays when evaluating contrast.

- seed:

  Random seed for reproducibility.

- weight_transform:

  Optional weight adjustment callback passed to
  \[build_conflict_edges()\].

## Value

Tibble with ROI IDs and assigned colours.

## Examples

``` r
# \donttest{
rois <- data.frame(
  roi = 1:10,
  x = runif(10, 0, 90), y = runif(10, 0, 100), z = runif(10, 0, 80)
)
pal <- roi_colors_maximin_view(rois, xyz_cols = c("x", "y", "z"))
# }
```
