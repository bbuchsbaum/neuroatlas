# Validate an Atlas Reference

Validate an Atlas Reference

## Usage

``` r
validate_atlas_ref(x)
```

## Arguments

- x:

  Object to validate.

## Value

Invisibly returns \`x\` when valid.

## Examples

``` r
ref <- new_atlas_ref("toy", "two-regions",
  template_space = "MNI152NLin6Asym", coord_space = "MNI152"
)
validate_atlas_ref(ref)
```
