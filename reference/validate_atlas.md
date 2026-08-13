# Validate an Atlas Object

Cheap structural validator used by \[new_atlas()\] and
\[new_surfatlas()\]. Raises a classed \`neuroatlas_error_invalid_atlas\`
condition via \[cli::cli_abort()\] when required fields are missing or
have inconsistent lengths.

## Usage

``` r
validate_atlas(x)
```

## Arguments

- x:

  An atlas object.

## Value

Invisibly returns \`x\` when valid.
