# Sample a Volume onto an Explicit Cortical Projection

Scalar and probability data use trilinear interpolation; labels use
nearest voxel centers with lower-index half-voxel ties. Sampling is
restricted to the closed voxel-center box; unsupported values are
\`NA\`, and supported zero is preserved. Probability channels retain
partial mass and are not normalized across maps. Missingness considers
strictly positive contributors only. fsLR composition resolves sampling
missingness on fsaverage first, then resolves missing surface
contributors with the native operator. Coverage for each stage is
retained separately. Ribbon omission combines voxel and node weights
within the sampling stage.

## Usage

``` r
apply_surface_projection(
  x,
  projection,
  source_space = NULL,
  affine = NULL,
  data_type = c("auto", "continuous", "label", "probability"),
  na_policy = c("propagate", "omit", "error"),
  label_table = NULL
)
```

## Arguments

- x:

  A 3D/4D numeric array, \`NeuroVol\`, \`NeuroVec\`, or volumetric
  atlas.

- projection:

  A \`SurfaceProjection\` from \[get_surface_projection()\] or
  \[ribbon_projection()\].

- source_space:

  Exact frame identifier. Required for unannotated input; an explicit
  value must agree with attached metadata and the projection.

- affine:

  For arrays, a finite nonsingular 4-by-4 matrix mapping zero-based
  voxel indices to RAS millimetres. Neuroimaging objects supply their
  own affine.

- data_type:

  \`"auto"\`, \`"continuous"\`, \`"label"\`, or \`"probability"\`.
  Unannotated input requires an explicit type.

- na_policy:

  \`"propagate"\`, \`"omit"\` with finite-weight normalization, or
  \`"error"\` for a missing positive contributor in included cortical
  support. Outside-grid samples remain unsupported under every policy.

- label_table:

  Optional key/name/color table preserved on output.

## Value

\`SurfaceData\` with values, exact target domain, per-map availability,
finite source-weight mass, geometric sampling coverage, lost labels and
directed projection provenance. Ribbon omission normalizes all finite
voxel/node weights together; categorical nodes vote with smallest-key
ties.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- apply_surface_projection(volume, projection,
  source_space = "MNI152NLin6Asym", data_type = "probability"
)
} # }
```
