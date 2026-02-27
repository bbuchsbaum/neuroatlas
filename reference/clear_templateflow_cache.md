# Clear neuroatlas TemplateFlow Cache

Removes cached files and clears in-memory memoisation.

## Usage

``` r
clear_templateflow_cache(confirm = TRUE)
```

## Arguments

- confirm:

  Logical. If \`TRUE\` (the default), asks for interactive confirmation.

## Value

Invisibly returns \`TRUE\` if the cache was cleared, \`FALSE\` if
aborted.

## Examples

``` r
if (FALSE) { # \dontrun{
  clear_templateflow_cache()
  clear_templateflow_cache(confirm = FALSE)
} # }
```
