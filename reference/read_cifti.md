# Read CIFTI-2 Dense Scalar or Label Maps

Preserves both cortical and volumetric brain models, their zero-based
indices, map metadata and per-map label tables. Matrix axes are
normalized in memory only: brainordinates are rows and maps are columns.
The original file layout is retained for writing. CIFTI metadata does
not identify a registration sphere or an exact MNI template; vertex
counts alone cannot bind a surface domain.

## Usage

``` r
read_cifti(file)
```

## Arguments

- file:

  Path to a CIFTI-2 dense scalar or dense label file.

## Value

A `CiftiData` S3 list with `values`, ordered `brain_models`, `volume`,
`map_names`, `label_tables`, original XML/header/extensions and file
identity. `available` distinguishes missing or explicitly unassigned
samples from supported zero. Vertex counts do not prove a surface
registration identity. Scalar missing values are retained. Infinite
values and label values absent from their map's label table are
rejected. Series/connectivity/parcellated mappings and CIFTI-1 are not
supported.

## See also

[`write_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/write_cifti.md),
[`replace_cifti_values()`](https://bbuchsbaum.github.io/neuroatlas/reference/replace_cifti_values.md)

## Examples

``` r
# Read an existing file without guessing a surface registration:
if (FALSE) { # \dontrun{
read_cifti("atlas.dlabel.nii")
} # }
```
