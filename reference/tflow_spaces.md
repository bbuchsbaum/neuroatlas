# List Available TemplateFlow Template Spaces

Retrieves a list of all available template space identifiers from the
TemplateFlow archive. These identifiers are top-level names like
"MNI152NLin2009cAsym", "fsLR", etc.

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

  (Optional) An existing \`templateflow\` S3 object created by
  \`create_templateflow()\`. If \`NULL\`, a default one will be
  initialized.

- ...:

  Additional arguments passed to \`grep\` if \`pattern\` is specified
  (e.g., \`ignore.case = TRUE\`).

## Value

A character vector of available template space names. Returns \`NULL\`
if the list cannot be retrieved, with a warning.

## Examples

``` r
# \donttest{
  # List all template spaces
  # all_spaces <- tflow_spaces()
  # print(head(all_spaces))

  # List template spaces containing "MNI"
  # mni_spaces <- tflow_spaces(pattern = "MNI")
  # print(mni_spaces)
# }
```
