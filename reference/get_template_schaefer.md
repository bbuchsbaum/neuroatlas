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
if (reticulate::py_available(initialize = TRUE) &&
    reticulate::py_module_available("templateflow")) {
  tryCatch(result <- get_template_schaefer(), error = function(e) NULL)
}
# }
```
