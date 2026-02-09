# Network-harmonic palette with neighbour separation

Assign colours so that networks share analogous hue families while still
maximising spatial separation between neighbouring ROIs.

## Usage

``` r
roi_colors_network_harmony(
  rois,
  id_col = "roi",
  xyz_cols = c("x", "y", "z"),
  network_col = "network",
  hemi_col = NULL,
  k = 12,
  sigma_xy = 25,
  sigma_slice = 10,
  scheme = "even",
  start_hue = 15,
  hue_width = 28,
  candidate_multiplier = 8,
  tau = 10,
  lambda_global = 0.1,
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

- network_col:

  Optional network label column to slightly boost cross-network
  conflicts.

- hemi_col:

  Optional hemisphere column used for the final light/dark adjustment.

- k:

  Number of nearest neighbours to evaluate when forming edges.

- sigma_xy:

  In-plane decay (mm) for slice visibility.

- sigma_slice:

  Through-slice decay (mm) for slice visibility.

- scheme:

  Hue spacing scheme passed to \[network_anchor_hues()\].

- start_hue:

  Starting hue in degrees (0–360).

- hue_width:

  Half-width (degrees) of the analogous band around each anchor hue.

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

Tibble with ROI IDs and hex colours.

## Examples

``` r
# \donttest{
rois <- data.frame(
  roi = 1:10, network = rep(c("Vis", "DMN"), 5),
  x = runif(10, 0, 90), y = runif(10, 0, 100), z = runif(10, 0, 80)
)
pal <- roi_colors_network_harmony(rois, xyz_cols = c("x","y","z"),
                                   network_col = "network")
# }
```
