# Get CSF Probability Map (DEPRECATED)

**DEPRECATED:** Please use
[`get_template`](get_template.md)`(variant = "probseg", label = "CSF", ...)`
instead.

Convenience function to get CSF probability map.

## Usage

``` r
get_template_csf(
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

A NeuroVol object containing the CSF probability map

## See also

The new [`get_template`](get_template.md)

## Examples

``` r
# \donttest{
if (reticulate::py_available(initialize = TRUE) &&
    reticulate::py_module_available("templateflow")) {
  tryCatch(result <- get_template_csf(), error = function(e) NULL)
}
#> Warning: The `name` argument of `get_template_csf()` is deprecated as of neuroatlas
#> 0.10.0.
#> i Please use `get_template()` instead.
#> NULL
# }
```
