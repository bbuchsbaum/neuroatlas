# Create a TemplateFlow Interface Object

Initializes and returns an S3 object of class `templateflow` which acts
as a gateway to the TemplateFlow Python API and manages configurations.

## Usage

``` r
create_templateflow(cache_dir = NULL, verbosity = 0, default_template = NULL)
```

## Arguments

- cache_dir:

  Optional. Path to a directory for caching TemplateFlow downloads. If
  NULL (default), uses a neuroatlas-specific cache directory obtained
  via `.neuroatlas_cache_dir("templateflow")`. This will also set the
  `TEMPLATEFLOW_HOME` environment variable for the Python session.

- verbosity:

  Optional. An integer for verbosity level (not yet implemented).

- default_template:

  Optional. A string for a default template to use (not yet
  implemented).

## Value

An S3 object of class `templateflow` containing:

- `api`: The raw Python TemplateFlow API handle from `reticulate`.

- `cache_path`: The R-side cache path being used.

- `options`: A list of user-provided options.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Ensure Python and templateflow module are available
  if (reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("templateflow")) {
    tf <- create_templateflow()
    print(tf)
  } else {
    message("Python or templateflow module not available. Skipping example.")
  }
} # }
```
