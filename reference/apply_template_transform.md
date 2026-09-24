# Apply a Template Transform on an Explicit Grid

Composes the complete route in physical coordinates and samples each
input channel once. Labels use nearest neighbour; scalar and probability
values use linear interpolation. Out-of-field samples are zero.
Probability channels are interpolated independently, without clipping or
renormalisation.

## Usage

``` r
apply_template_transform(
  x,
  transform,
  target,
  data_type = c("auto", "continuous", "label", "probability"),
  interpolation = NULL
)
```

## Arguments

- x:

  A volumetric atlas, \`NeuroVol\`, or \`NeuroVec\` (channels in
  dimension 4).

- transform:

  A verified \[get_template_transform()\] result.

- target:

  Target atlas, \`NeuroVol\`, or explicit \`NeuroSpace\`. A bare grid is
  an assertion by the caller that it is in the transform's target space.
  Attached source/target metadata must agree with the route.

- data_type:

  \`"auto"\` uses declared metadata, or label semantics for atlas
  objects. Unannotated volumes require an explicit type; integer-valued
  samples alone do not imply labels.

- interpolation:

  \`NULL\` selects the type-specific method. Only \`"nearest"\` for
  labels and \`"linear"\` for continuous/probability data are supported.

## Value

The transformed atlas or volume. The \`neuroatlas_transform\` attribute
records the route, artifact hashes, interpolation, grids and lost
labels. Atlas objects retain semantic IDs, labels and source provenance,
including regions that disappear on the target grid.
