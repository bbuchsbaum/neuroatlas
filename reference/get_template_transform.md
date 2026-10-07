# Resolve and Load a Verified Template Transform

Resolves an available image or exact-domain surface route and loads its
verified artifacts. Two `SurfaceGeometry` endpoints select an admitted
native surface operator. An exact MNI identifier and a pinned surface
target select a directed population cortical projection. Equal domains
require a verified diagonal operator with unit weights before admission
as an identity. Planning and fitting are separate: this function never
estimates a registration. Nonlinear routes must have a published
checksum and a passing qualification record in
[`space_transform_manifest()`](https://bbuchsbaum.github.io/neuroatlas/reference/space_transform_manifest.md).

## Usage

``` r
get_template_transform(
  from,
  to,
  provider = c("auto", "neuroatlas", "templateflow"),
  download = TRUE,
  verify = TRUE,
  cache_dir = transform_cache_path(),
  offline = FALSE,
  cortex = NULL,
  volume_space = NULL
)
```

## Arguments

- from, to:

  Exact template identifiers, verified `SurfaceGeometry` objects or
  `CiftiData` layouts with explicitly supplied operators and volume
  frame. Surface routes require exact geometries; broad names cannot
  establish vertex ordering or admit execution.

- provider:

  Artifact provider, or `"auto"` for registry selection.

- download:

  Allow missing artifacts to be downloaded.

- verify:

  Must be `TRUE`; artifact integrity cannot be disabled.

- cache_dir:

  Dedicated transform cache directory.

- offline:

  Use only verified local artifacts.

- cortex:

  For two `CiftiData` layouts, explicitly bound qualified cortical
  operators passed to
  [`get_cifti_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_cifti_transform.md).
  Other representations require `NULL`.

- volume_space:

  For CIFTI layouts with voxels, the caller-declared common exact frame,
  passed to
  [`get_cifti_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_cifti_transform.md).
  Other representations require `NULL`.

## Value

A `template_transform` containing the plan, files, pullback morphism,
and artifact provenance. Its direction describes image movement; its
morphism maps target coordinates into source coordinates for sampling.
Surface routes return the operator from
[`get_surface_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_surface_transform.md)
or the `SurfaceProjection` from
[`get_surface_projection()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_surface_projection.md).
CIFTI layouts return a `CiftiTransform` from
[`get_cifti_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_cifti_transform.md),
preserving noncortical geometry and support.

## See also

[`apply_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_template_transform.md),
[`transform_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/transform_atlas.md)

## Examples

``` r
if (requireNamespace("neurotransform", quietly = TRUE) &&
  requireNamespace("hdf5r", quietly = TRUE)) {
  get_template_transform("MNI152NLin6Asym", "MNI152NLin6Asym")
}
#> $from_space
#> [1] "MNI152NLin6Asym"
#> 
#> $to_space
#> [1] "MNI152NLin6Asym"
#> 
#> $plan
#> <atlas_transform_plan>
#>   from_space: MNI152NLin6Asym 
#>   to_space: MNI152NLin6Asym 
#>   n_steps: 1 
#>   status: available 
#>   confidence: exact 
#> 
#> $files
#> [1] NA
#> 
#> $morphism
#> <IdentityMorphism id_MNI152NLin6Asym | MNI152NLin6Asym -> MNI152NLin6Asym | kind=identity | method=identity | inverse=exact | cost=0.000>
#> 
#> $cache_dir
#> [1] "/home/runner/.cache/R/neuroatlas/transforms"
#> 
#> $engine_version
#> [1] "0.2.0"
#> 
#> $engine_source_sha
#> [1] "933edddda462593941e167726e8aaa7168ff103a"
#> 
#> $engine_compatibility
#> [1] "simpleitk-h5-conventions-v1"
#> 
#> attr(,"class")
#> [1] "template_transform" "list"              
```
