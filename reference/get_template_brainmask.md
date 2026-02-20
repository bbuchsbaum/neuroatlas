# Get Brain Mask from Template (DEPRECATED)

**DEPRECATED:** Please use
[`get_template`](get_template.md)`(variant = "mask", ...)` instead.

Convenience function to retrieve a binary brain mask for a specified
template.

## Usage

``` r
get_template_brainmask(
  name = "MNI152NLin2009cAsym",
  resolution = 1,
  extension = ".nii.gz"
)
```

## Arguments

- name:

  Character string specifying template name. Default:
  "MNI152NLin2009cAsym"

- resolution:

  Numeric resolution in mm. Default: 1

- extension:

  Character string specifying file extension. Default: ".nii.gz"

## Value

A NeuroVol object containing the binary brain mask

## See also

The new [`get_template`](get_template.md)

## Examples

``` r
# \donttest{
if (reticulate::py_available(initialize = TRUE) &&
    reticulate::py_module_available("templateflow")) {
  tryCatch(result <- get_template_brainmask(), error = function(e) NULL)
}
#> Downloading uv...
#> Done!
# }
```
