# Apply a Verified Template or Surface Transform

Composes the complete route in physical coordinates and samples each
input channel once. Labels use nearest neighbour; scalar and probability
values use linear interpolation. For image-to-image transforms,
out-of-field samples are zero and probability channels are interpolated
independently. Surface transforms dispatch to
[`apply_surface_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_surface_transform.md)
or
[`apply_surface_projection()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_surface_projection.md),
whose unsupported samples are `NA` and whose coverage and missingness
semantics are documented separately.

## Usage

``` r
apply_template_transform(
  x,
  transform,
  target = NULL,
  data_type = c("auto", "continuous", "label", "probability"),
  interpolation = NULL,
  missing_labels = NULL
)
```

## Arguments

- x:

  A volumetric atlas, `NeuroVol`, or `NeuroVec` (channels in dimension
  4), or `SurfaceData` for a native surface operator. A mixed cortical
  adapter requires `CiftiData` with its bound source layout.

- transform:

  A verified
  [`get_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_template_transform.md)
  result.

- target:

  Target atlas, `NeuroVol`, or explicit `NeuroSpace`. A bare grid is an
  assertion by the caller that it is in the transform's target space.
  Attached source/target metadata must agree with the route. Surface
  operators already bind their exact target domain: `NULL` uses that
  domain, or supply the matching `SurfaceGeometry` or `SurfaceDomain`.

- data_type:

  `"auto"` uses declared metadata, or label semantics for atlas objects.
  Unannotated volumes require an explicit type; integer-valued samples
  alone do not imply labels.

- interpolation:

  `NULL` selects the type-specific method. Only `"nearest"` for labels
  and `"linear"` for continuous/probability data are supported.

- missing_labels:

  Explicit unassigned keys for unsupported CIFTI label samples, passed
  to
  [`apply_cifti_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_cifti_transform.md).
  Other representations require `NULL`.

## Value

The transformed atlas or volume. The `neuroatlas_transform` attribute
records the route, artifact hashes, interpolation, grids and lost
labels. Atlas objects retain semantic IDs, labels and source provenance,
including regions that disappear on the target grid. A surface
destination returns `SurfaceData` with exact domain, availability and
projection provenance. A CIFTI destination returns `CiftiData` and
preserves noncortical support.

## Examples

``` r
if (requireNamespace("neurotransform", quietly = TRUE) &&
  requireNamespace("hdf5r", quietly = TRUE)) {
  grid <- neuroim2::NeuroSpace(c(2, 2, 2))
  volume <- neuroim2::NeuroVol(array(0.25, c(2, 2, 2)), grid)
  transform <- get_template_transform("MNI152NLin6Asym", "MNI152NLin6Asym")
  apply_template_transform(volume, transform, grid, data_type = "continuous")
}
#> <DenseNeuroVol> [52.1 Kb] 
#> ── Spatial ───────────────────────────────────────────────────────────────────── 
#>   Dimensions    : 2 x 2 x 2
#>   Spacing       : 1 x 1 x 1 mm
#>   Origin        : 0, 0, 0
#>   Orientation   : RAS
#> ── Data ──────────────────────────────────────────────────────────────────────── 
#>   Range         : [0.250, 0.250]
```
