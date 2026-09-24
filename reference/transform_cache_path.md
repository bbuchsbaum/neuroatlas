# Transform Artifact Cache Path

Returns the directory reserved for verified transform artifacts. This
helper does not create the directory.

## Usage

``` r
transform_cache_path(cache_dir = tools::R_user_dir("neuroatlas", "cache"))
```

## Arguments

- cache_dir:

  Base cache directory.

## Value

A character path to the transform cache directory.
