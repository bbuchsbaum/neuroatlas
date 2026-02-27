# Find TemplateFlow Files Matching Metadata Criteria

Retrieves a list of file paths from TemplateFlow that match a given
template space and other optional metadata query parameters.

## Usage

``` r
tflow_files(space, query_args = list(), api_handle = NULL)
```

## Arguments

- space:

  Character string. The TemplateFlow identifier for the template space
  (e.g., "MNI152NLin2009cAsym").

- query_args:

  (Optional) A named list of additional query parameters to filter the
  results (e.g., \`list(suffix = "T1w", resolution = 1, desc =
  "brain")\`).

- api_handle:

  Deprecated and ignored.

## Value

A character vector of file paths matching the query. Returns an empty
vector if no files match.

## Examples

``` r
# \donttest{
  # List all T1w files for MNI152NLin2009cAsym template
  # mni_t1w_files <- tflow_files("MNI152NLin2009cAsym",
  #                              query_args = list(suffix = "T1w"))
# }
```
