# Print a TemplateFlow Object

Provides a brief summary of the TemplateFlow interface object.

## Usage

``` r
# S3 method for class 'templateflow'
print(x, ...)
```

## Arguments

- x:

  An object of class `templateflow`.

- ...:

  Additional arguments (unused).

## Value

The input object `x`, returned invisibly.

## Examples

``` r
# \donttest{
if (reticulate::py_available(initialize = TRUE) &&
    reticulate::py_module_available("templateflow")) {
  tryCatch({tf <- create_templateflow(); print(tf)},
    error = function(e) NULL)
}
#> <neuroatlas TemplateFlow Interface>
#>   Cache Path:  /Users/bbuchsbaum/Library/Caches/org.R-project.R/R/neuroatlas/templateflow 
#>   API Status:  Connected (Python API handle initialized) 
#>   Available Templates (Examples):  MNI152Lin, MNI152NLin2009cAsym, VALiDATe29, dhcpAsym, fsLR, ... (Total: 6)
# }
```
