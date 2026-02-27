# Show neuroatlas TemplateFlow Cache Path

Returns the path to the TemplateFlow cache directory. With the pure R
`templateflow` package, this is the `TEMPLATEFLOW_HOME` directory.

## Usage

``` r
show_templateflow_cache_path()
```

## Value

A character string representing the path to the TemplateFlow cache
directory.

## Examples

``` r
cat("TemplateFlow cache is at:", show_templateflow_cache_path(), "\n")
#> TemplateFlow cache is at: /Users/bbuchsbaum/Library/Caches/org.R-project.R/R/neuroatlas/templateflow 
```
