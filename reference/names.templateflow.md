# List Attributes of the TemplateFlow API Object

Lists the names of attributes and methods available on the underlying
Python TemplateFlow API object.

## Usage

``` r
# S3 method for class 'templateflow'
names(x)
```

## Arguments

- x:

  An object of class `templateflow`.

## Value

A character vector of available attribute and method names.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Ensure Python and templateflow module are available
  if (reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("templateflow")) {
    tf <- create_templateflow()
    # print(names(tf))
  } else {
    message("Python or templateflow module not available. Skipping example.")
  }
} # }
```
