# Initialize and Get TemplateFlow API Handle

Establishes connection to the Python TemplateFlow API via reticulate.
Stores the API handle and cache path in an internal environment. It also
sets the TEMPLATEFLOW_HOME environment variable to ensure the Python
library uses the neuroatlas-managed cache directory.

## Usage

``` r
.init_templateflow_api(cache_dir = NULL, force_reinit = FALSE)
```

## Arguments

- cache_dir:

  The directory to use for caching TemplateFlow files managed by
  neuroatlas. Defaults to \`.neuroatlas_cache_dir("templateflow")\`. If
  provided, this path will be used for TEMPLATEFLOW_HOME.

- force_reinit:

  Logical, whether to force re-initialization of the Python API handle.

## Value

Invisibly returns the TemplateFlow S3 object.
