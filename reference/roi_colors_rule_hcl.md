# Deterministic HCL palette with harmonic variation

Generates a fast, reproducible palette that combines harmonic network
hues with anterior-posterior gradients and hemisphere luminance
differences.

## Usage

``` r
roi_colors_rule_hcl(
  rois,
  id_col = "roi",
  xyz_cols = c("x", "y", "z"),
  network_col = NULL,
  hemi_col = NULL,
  scheme = "even",
  start_hue = 15,
  hue_width = 30,
  C = 70,
  L_L = 72,
  L_R = 60
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

- scheme:

  Hue spacing scheme passed to \[network_anchor_hues()\].

- start_hue:

  Starting hue in degrees (0–360).

- hue_width:

  Half-width (degrees) of within-network hue modulation.

- C:

  Fixed chroma.

- L_L:

  Lightness for left hemisphere ROIs.

- L_R:

  Lightness for right hemisphere ROIs.

## Value

Tibble with ROI IDs and colours.

## Examples

``` r
# \donttest{
rois <- data.frame(
  roi = 1:10, network = rep(c("Vis", "DMN"), 5),
  hemi = rep(c("left", "right"), 5),
  x = runif(10), y = runif(10), z = runif(10)
)
pal <- roi_colors_rule_hcl(rois, network_col = "network",
                            hemi_col = "hemi", xyz_cols = c("x","y","z"))
# }
```
