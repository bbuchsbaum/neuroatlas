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
if (reticulate::py_available(initialize = TRUE) &&
    reticulate::py_module_available("templateflow")) {
  tryCatch({available <- templates(); head(available)},
    error = function(e) NULL)
}
#> Warning: `templates()` was deprecated in neuroatlas 0.10.0.
#> ℹ Please use `tflow_spaces()` instead.
#> NULL
# }
```
