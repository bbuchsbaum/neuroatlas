# Find TemplateFlow Files Matching Metadata Criteria

Retrieves a list of file paths from TemplateFlow that match a given
template space and other optional metadata query parameters. This
function calls the Python \`templateflow.api.get()\` method with
\`raise_on_empty=FALSE\` to get a list of all matching files.

## Usage

``` r
tflow_files(space, query_args = list(), api_handle = NULL)
```

## Arguments

- space:

  Character string. The primary TemplateFlow identifier for the template
  space (e.g., "MNI152NLin2009cAsym"). This is passed as \`template\` to
  the Python API.

- query_args:

  (Optional) A named list of additional query parameters to filter the
  results (e.g., \`list(suffix = "T1w", resolution = "1", desc =
  "brain")\`). These are passed directly as keyword arguments to the
  Python \`templateflow.api.get()\`.

- api_handle:

  (Optional) An existing \`templateflow\` S3 object created by
  \`create_templateflow()\`. If \`NULL\`, a default one will be
  initialized.

## Value

A character vector of file paths matching the query. Returns an empty
vector if no files match, or \`NULL\` with a warning if the API call
fails.

## Examples

``` r
# \donttest{
  # List all T1w files for MNI152NLin2009cAsym template
  # mni_t1w_files <- tflow_files("MNI152NLin2009cAsym",
  #                                           query_args = list(suffix = "T1w"))
  # print(mni_t1w_files)

  # List all files for the OASIS30ANTs template with desc "brain"
  # oasis_brains <- tflow_files("OASIS30ANTs",
  #                                           query_args = list(desc = "brain"))
  # print(oasis_brains)
# }
```
