# Infer Design Variable Type

Classify a vector as `"continuous"` or `"categorical"` for plot
defaults.

## Usage

``` r
infer_design_var_type(x)
```

## Arguments

- x:

  A vector.

## Value

A character scalar.

## Examples

``` r
infer_design_var_type(c(0.1, 0.2, 0.3))
#> [1] "continuous"
infer_design_var_type(c("control", "patient"))
#> [1] "categorical"
```
