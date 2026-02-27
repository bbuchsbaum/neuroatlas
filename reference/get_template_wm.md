# Get White Matter Probability Map (DEPRECATED)

**DEPRECATED:** Please use
[`get_template`](get_template.md)`(variant = "probseg", label = "WM", ...)`
instead.

Convenience function to get white matter probability map.

## Usage

``` r
get_template_wm(
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

A NeuroVol object containing the white matter probability map

## See also

The new [`get_template`](get_template.md)

## Examples

``` r
# \donttest{
if (requireNamespace("templateflow", quietly = TRUE)) {
  tryCatch(result <- get_template_wm(), error = function(e) NULL)
}
#> Warning: The `name` argument of `get_template_wm()` is deprecated as of neuroatlas
#> 0.10.0.
#> ℹ Please use `get_template()` instead.
# }
```
