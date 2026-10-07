# Bind Qualified Cortical Operators to a CIFTI Layout

Creates a mixed-layout adapter with explicit hemisphere operators. The
file cannot prove a registration identity: supplying an operator
declares that its exact source domain matches the CIFTI vertex ordering.
Counts are checked but never used to infer that binding. Noncortical
models must retain the same indices and volume geometry; their rows may
be reordered. Changed voxel grids or support require a separate
volumetric operation and are rejected here.

## Usage

``` r
get_cifti_transform(from, to, cortex, volume_space = NULL)
```

## Arguments

- from:

  Source `CiftiData`, used for its brain-model layout.

- to:

  Target `CiftiData`, used for its brain-model layout only. Source map
  names, metadata and label tables are retained during application.

- cortex:

  Named list of qualified `SurfaceTransform` operators, `L` and/or `R`,
  covering every cortical hemisphere present. Obtain these with
  [`get_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_template_transform.md)
  and explicitly verified surface geometries.

- volume_space:

  Caller-declared exact volume template shared by source and target,
  required when voxel models are present. Supply one common identifier,
  or a named `c(from = ..., to = ...)` pair that must agree. This first
  adapter accepts the qualified MNI6/MNI2009c frames only. Affines
  cannot establish template identity. Declaring a frame does not
  establish anatomical accuracy.

## Value

A `CiftiTransform` binding source/target layouts and cortical operators.

## See also

[`apply_cifti_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_cifti_transform.md),
[`read_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/read_cifti.md)

## Examples

``` r
if (FALSE) { # \dontrun{
op <- get_cifti_transform(source, reference, list(L = left, R = right),
  volume_space = "MNI152NLin6Asym"
)
} # }
```
