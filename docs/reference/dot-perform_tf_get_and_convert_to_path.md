# Perform TemplateFlow API Call and Convert to Path

Internal function that calls the Python TemplateFlow API's get() method,
handles errors, and converts the result (potentially a list of paths) to
a single R file path string. If multiple paths are returned by
TemplateFlow, a warning is issued and the first path is used.

## Usage

``` r
.perform_tf_get_and_convert_to_path(tf_api_obj, query_params_list)
```

## Arguments

- tf_api_obj:

  The Python TemplateFlow API object from reticulate.

- query_params_list:

  A named list of query parameters for \`tf_api_obj\$get()\`.

## Value

A single character string representing the file path.
