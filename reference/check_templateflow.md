# Check TemplateFlow Installation Status

Checks whether the TemplateFlow R package is installed and provides
cache information.

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
#> templateflow R package: INSTALLED
#>   Version: 0.0.1 
#>   Cache info: unavailable ( 'tf_home' is not an exported object from 'namespace:templateflow' )
#> 
#>   Templates available: 28 
#> 
#> === Status: Ready ===
#> TemplateFlow is properly installed and ready to use.
# }
```
