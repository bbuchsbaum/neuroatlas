# Inspect Coverage and Label Loss in a Cortical Projection

Returns diagnostics computed by
[`apply_surface_projection()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_surface_projection.md).
Label counts are separated by map and stage, so a key surviving another
map cannot hide loss. `not_sampled` means a source key has no supported
sampled vertex; `lost_resampling` means it survives sampling but
disappears during surface resampling. Declared keys absent from the
source have status `absent_source`. Opposite-hemisphere keys have status
`other_hemisphere` and are excluded from expected parcel-loss totals;
any supported output vertices carrying those keys are still counted as
hemisphere mismatches. Without explicit hemisphere metadata, keys remain
unchecked and no hemisphere is inferred. Key zero is counted separately
and excluded from parcel-loss totals; supported zero is still a valid
value. Diagnostics never mask, relabel or repair values.

## Usage

``` r
projection_diagnostics(x)
```

## Arguments

- x:

  A `SurfaceData` result from
  [`apply_surface_projection()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_surface_projection.md).

## Value

A list with `summary`, a per-map coverage/count tibble, and `labels`, a
per-map/per-key tibble for label data (`NULL` for other data types).
Label rows contain source voxel counts, supported sampled and target
vertex counts, hemisphere declarations, loss flags and stage status.
Vertex and voxel counts are not interchangeable area or volume measures.
Missing and masked vertices are excluded from label counts and retained
in coverage.

## Examples

``` r
if (FALSE) { # \dontrun{
qa <- projection_diagnostics(projected_atlas)
qa$summary
subset(qa$labels,
  lost_at_sampling | lost_at_resampling | hemisphere_mismatch
)
} # }
```
