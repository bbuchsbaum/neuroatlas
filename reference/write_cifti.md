# Write CIFTI-2 Maps With Preserved Brain Models

Writes double-precision values using the original axis layout, XML
metadata, brain models, label tables and other NIfTI extensions. Label
missing values are rejected; no background label is assigned implicitly.

## Usage

``` r
write_cifti(x, file, overwrite = FALSE)
```

## Arguments

- x:

  A valid `CiftiData`.

- file:

  Destination ending in `.nii` (uncompressed CIFTI-2).

- overwrite:

  Allow replacement of an existing regular file.

## Value

The normalized destination path, invisibly.

## See also

[`read_cifti()`](https://bbuchsbaum.github.io/neuroatlas/reference/read_cifti.md),
[`replace_cifti_values()`](https://bbuchsbaum.github.io/neuroatlas/reference/replace_cifti_values.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- read_cifti("atlas.dlabel.nii")
write_cifti(x, "copy.dlabel.nii")
} # }
```
