# Replace Values Without Changing CIFTI Brain Models

Keeps brain-model rows, map metadata and label tables bound to new
values. Changing indices or map counts requires a new explicitly
constructed layout. Previously unavailable samples remain unavailable,
even when their replacement values are finite or use an existing label
key.

## Usage

``` r
replace_cifti_values(x, values)
```

## Arguments

- x:

  A `CiftiData` from
  [`read_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/read_cifti.md).

- values:

  Numeric matrix with the same dimensions as `x$values`.

## Value

A new `CiftiData`, with the input identity recorded in provenance.

## See also

[`read_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/read_cifti.md),
[`write_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/write_cifti.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- read_cifti("map.dscalar.nii")
x <- replace_cifti_values(x, x$values * 2)
} # }
```
