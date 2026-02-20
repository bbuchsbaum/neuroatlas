# Show neuroatlas TemplateFlow Cache Path

Returns the path to the \`neuroatlas\` package's cache directory used
for TemplateFlow downloads. This is typically located within the path
returned by \`tools::R_user_dir("neuroatlas", "cache")\`, in a
subdirectory named "templateflow".

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
#> TemplateFlow cache is at: /home/runner/.cache/R/neuroatlas/templateflow 
```
