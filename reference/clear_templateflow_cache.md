# Clear neuroatlas TemplateFlow Cache

Removes all files and subdirectories from the \`neuroatlas\` package's
cache directory used for TemplateFlow downloads. This function also
clears the in-memory memoisation cache for TemplateFlow path lookups.

## Usage

``` r
clear_templateflow_cache(confirm = TRUE)
```

## Arguments

- confirm:

  Logical. If \`TRUE\` (the default), the function will ask for
  interactive confirmation before deleting files if the session is
  interactive. If \`FALSE\`, or if the session is not interactive,
  deletion will proceed without confirmation.

## Value

Invisibly returns \`TRUE\` if the cache was cleared or attempted to be
cleared, and \`FALSE\` if the operation was aborted by the user during
confirmation.

## Details

The TemplateFlow cache directory is typically located within the path
returned by \`tools::R_user_dir("neuroatlas", "cache")\`, in a
subdirectory named "templateflow".

## Examples

``` r
if (FALSE) { # \dontrun{
  # Clear the TemplateFlow cache (will ask for confirmation if interactive)
  # clear_templateflow_cache()

  # Clear without confirmation
  # clear_templateflow_cache(confirm = FALSE)
} # }
```
