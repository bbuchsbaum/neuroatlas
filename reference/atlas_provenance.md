# Atlas Provenance Accessors

Access structured provenance metadata for atlas objects, including the
canonical atlas identity, upstream artifacts, and processing history.

## Usage

``` r
atlas_provenance(x, ...)

# S3 method for class 'atlas'
atlas_provenance(x, ...)

# Default S3 method
atlas_provenance(x, ...)

# S3 method for class 'atlas'
atlas_artifacts(x, ...)

# Default S3 method
atlas_artifacts(x, ...)

# S3 method for class 'atlas'
atlas_history(x, ...)

# Default S3 method
atlas_history(x, ...)
```

## Arguments

- x:

  An atlas object.

- ...:

  Additional arguments passed to methods.

## Value

A list of class \`"atlas_provenance"\` with fields:

- ref:

  Canonical [`atlas_ref()`](atlas_ref.md) identity metadata.

- artifacts:

  A tibble describing upstream files/resources.

- history:

  A tibble describing processing steps applied in `neuroatlas`.
