# Get Template Head Image (DEPRECATED)

**DEPRECATED:** Please use
[`get_template`](get_template.md)`(variant = "head", ...)` instead.

Convenience function to get the full head (non-brain-extracted)
template.

## Usage

``` r
get_template_head(
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

A NeuroVol object containing the head template

## See also

The new [`get_template`](get_template.md)

## Examples

``` r
# \donttest{
if (reticulate::py_available(initialize = TRUE) &&
    reticulate::py_module_available("templateflow")) {
  tryCatch(result <- get_template_head(), error = function(e) NULL)
}
#> Warning: The `name` argument of `get_template_head()` is deprecated as of neuroatlas
#> 0.10.0.
#> i Please use `get_template()` instead.
#> NULL
# }
```
