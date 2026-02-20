# Build slice-aware conflict edges between ROIs

Constructs a sparse, symmetric edge list describing which ROIs are
likely to collide visually across common slice views. The resulting
tibble records the ROI indices, names, and a normalised conflict weight
in \\0, 1\\.

## Usage

``` r
build_conflict_edges(
  rois,
  id_col = "roi",
  xyz_cols = c("x", "y", "z"),
  k = 12,
  sigma_3d = NULL,
  views = c("axial", "coronal", "sagittal"),
  view_weights = c(axial = 1, coronal = 1, sagittal = 1),
  sigma_xy = 25,
  sigma_slice = 10,
  hemi_col = NULL,
  cross_hemi_factor = 0.85,
  network_col = NULL,
  diff_network_factor = 1.15,
  weight_transform = NULL
)
```

## Arguments

- rois:

  Tibble with ROI metadata. Must contain \`id_col\` and the three
  columns listed in \`xyz_cols\`.

- id_col:

  Column containing ROI IDs.

- xyz_cols:

  Character vector of length three giving the coordinates to use for
  distance calculations.

- k:

  Number of nearest neighbours to evaluate when forming edges.

- sigma_3d:

  Spatial decay parameter in millimetres. Defaults to the median
  neighbour distance.

- views:

  Character vector of anatomical views to consider. Supported:
  \`"axial"\`, \`"coronal"\`, and \`"sagittal"\`.

- view_weights:

  Named numeric vector of weights for the supplied views.

- sigma_xy:

  In-plane decay (mm) for slice visibility.

- sigma_slice:

  Through-slice decay (mm) for slice visibility.

- hemi_col:

  Optional hemisphere column to down-weight cross-hemisphere conflicts.

- cross_hemi_factor:

  Multiplicative factor (\< 1) applied to conflicts involving opposite
  hemispheres.

- network_col:

  Optional network column to up-weight cross-network conflicts.

- diff_network_factor:

  Multiplicative factor (\> 1) applied to conflicts involving different
  networks.

- weight_transform:

  Optional function \`function(edges, rois)\` that can modify the
  weights before returning.

## Value

Tibble with columns \`from_idx\`, \`to_idx\`, \`from\`, \`to\`, and
\`w\`.

## Examples

``` r
# \donttest{
rois <- data.frame(
  roi = 1:6,
  x = c(10, 12, 50, 52, 10, 50),
  y = c(20, 22, 20, 22, 60, 60),
  z = c(30, 32, 30, 32, 30, 30)
)
edges <- build_conflict_edges(rois, xyz_cols = c("x", "y", "z"))
# }
```
