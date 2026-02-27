# Get Schaefer Parcellation in Template Space (DEPRECATED)

**DEPRECATED:** Please use
[`get_template`](get_template.md)`(atlas = "Schaefer2018", desc = ..., suffix = "dseg", ...)`
instead.

Retrieves Schaefer cortical parcellation mapped to a specified template
space.

## Usage

``` r
get_template_schaefer(
  name = "MNI152NLin2009cAsym",
  resolution = 1,
  parcels = 400,
  networks = 17,
  extension = ".nii.gz"
)
```

## Arguments

- name:

  Character string specifying template name. Default:
  "MNI152NLin2009cAsym"

- resolution:

  Numeric resolution in mm. Default: 1

- parcels:

  Number of parcels (400 default)

- networks:

  Number of networks (17 default)

- extension:

  Character string specifying file extension. Default: ".nii.gz"

## Value

A NeuroVol object containing the parcellation

## See also

The new [`get_template`](get_template.md)

## Examples

``` r
# \donttest{
if (requireNamespace("templateflow", quietly = TRUE)) {
  tryCatch(result <- get_template_schaefer(), error = function(e) NULL)
}
#> Warning: The `name` argument of `get_template_schaefer()` is deprecated as of neuroatlas
#> 0.10.0.
#> ℹ Please use `get_template()` instead.
# }
```
