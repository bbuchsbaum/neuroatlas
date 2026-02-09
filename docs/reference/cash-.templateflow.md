# Access Attributes of the TemplateFlow Object

Allows R-native access (via `$`) to attributes and methods of the
underlying Python TemplateFlow API object.

## Usage

``` r
# S3 method for class 'templateflow'
x$name
```

## Arguments

- x:

  An object of class `templateflow`.

- name:

  The name of the attribute or method to access on the Python object.

## Value

The attribute or method from the Python TemplateFlow API object.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Ensure Python and templateflow module are available
  if (reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("templateflow")) {
    tf <- create_templateflow()
    # Example: Access the 'get' method (it's a Python function)
    # print(tf$get)
    # Example: List available templates (calls tf$api$templates())
    # print(tf$templates())
  } else {
    message("Python or templateflow module not available. Skipping example.")
  }
} # }
```
