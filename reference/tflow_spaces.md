# List Available TemplateFlow Template Spaces

Retrieves a list of all available template space identifiers from the
TemplateFlow archive.

## Usage

``` r
tflow_spaces(pattern = NULL, api_handle = NULL, ...)
```

## Arguments

- pattern:

  (Optional) A character string containing a regular expression to
  filter the template space names. If \`NULL\` (default), all names are
  returned.

- api_handle:

  Deprecated and ignored.

- ...:

  Additional arguments passed to \`grep\` if \`pattern\` is specified
  (e.g., \`ignore.case = TRUE\`).

## Value

A character vector of available template space names.

## Examples

``` r
# \donttest{
  # List all template spaces
  # all_spaces <- tflow_spaces()

  # List template spaces containing "MNI"
  # mni_spaces <- tflow_spaces(pattern = "MNI")
# }
```
