# Check TemplateFlow Installation Status

Checks whether TemplateFlow and required dependencies are properly
installed and provides information about the Python environment being
used.

## Usage

``` r
check_templateflow()
```

## Value

Invisible NULL. Prints status information to the console.

## Examples

``` r
# \donttest{
check_templateflow()
#> === TemplateFlow Installation Status ===
#> 
#> Python: NOT AVAILABLE
#>   Run: reticulate::install_python()
#> 
# }
```
