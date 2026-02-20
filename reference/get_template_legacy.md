# Access Templateflow Brain Templates (DEPRECATED - Legacy Signature)

**DEPRECATED:** This function signature is deprecated. Please use the
new [`get_template`](get_template.md) function which offers a more
comprehensive and R-native interface. The new function handles common
variants and modalities more directly.

## Usage

``` r
get_template_legacy(
  name = "MNI152NLin2009cAsym",
  desc = "brain",
  resolution = 1,
  label = NULL,
  atlas = NULL,
  suffix = "T1w",
  extension = ".nii.gz"
)
```

## Arguments

- name:

  Character string specifying template name. Default:
  "MNI152NLin2009cAsym"

- desc:

  Character string describing template variant. Default: "brain"

- resolution:

  Numeric resolution in mm. Default: 1

- label:

  Character string specifying tissue label for probability maps

- atlas:

  Character string specifying atlas name

- suffix:

  Character string specifying image type. Default: "T1w"

- extension:

  Character string specifying file extension. Default: ".nii.gz"

## Value

A NeuroVol object containing the requested template

## See also

The new [`get_template`](get_template.md) with updated signature.

## Examples

``` r
# \donttest{
if (reticulate::py_available(initialize = TRUE) &&
    reticulate::py_module_available("templateflow")) {
  tryCatch(result <- get_template_legacy(), error = function(e) NULL)
}
# }
```
