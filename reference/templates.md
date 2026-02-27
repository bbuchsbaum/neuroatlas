# List Available Templates

Returns a list of all available templates in the Templateflow
repository.

## Usage

``` r
templates()
```

## Value

A character vector of available template names

## Examples

``` r
# \donttest{
if (requireNamespace("templateflow", quietly = TRUE)) {
  tryCatch({available <- templates(); head(available)},
    error = function(e) NULL)
}
#> Warning: `templates()` was deprecated in neuroatlas 0.10.0.
#> ℹ Please use `tflow_spaces()` instead.
#> [1] "Fischer344"          "MNI152Lin"           "MNI152NLin2009aAsym"
#> [4] "MNI152NLin2009aSym"  "MNI152NLin2009bAsym" "MNI152NLin2009bSym" 
# }
```
