# Apply Cortical Resampling While Preserving CIFTI Subcortex

Applies the declared qualified surface operator independently to each
map. Noncortical values and their availability are copied by
structure/index. Source map metadata and label tables are retained.
Missing scalar values stay missing. Missing label values require
explicit existing label keys, and their unavailable brainordinate
indices are retained in per-map XML metadata and `available`; assigning
a key does not establish sampled support.

## Usage

``` r
apply_cifti_transform(
  x,
  transform,
  na_policy = c("propagate", "omit", "error"),
  label_method = c("aggregate", "largest"),
  missing_labels = NULL
)
```

## Arguments

- x:

  Source `CiftiData` with the bound brain-model layout.

- transform:

  A `CiftiTransform` from
  [`get_cifti_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_cifti_transform.md).

- na_policy:

  Missing-value policy for cortical interpolation, passed to
  [`apply_surface_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_surface_transform.md).

- label_method:

  Categorical interpolation policy, passed to
  [`apply_surface_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_surface_transform.md).

- missing_labels:

  `NULL` rejects unsupported label samples. Otherwise, supply one
  existing label key per map, or a single key shared by every map.

## Value

A `CiftiData` on the target layout with original map metadata,
per-brainordinate availability and cortical execution provenance.

## See also

[`write_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/write_cifti.md),
[`apply_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_template_transform.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mapped <- apply_cifti_transform(source, op, missing_labels = 0)
write_cifti(mapped, "mapped.dlabel.nii")
} # }
```
