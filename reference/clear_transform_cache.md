# Clear Verified Transform Artifacts

Removes only transform artifact files owned by neuroatlas. The cache
root is retained, and a cache containing an active artifact lock is
refused.

## Usage

``` r
clear_transform_cache(
  artifact_version = NULL,
  cache_dir = transform_cache_path()
)
```

## Arguments

- artifact_version:

  Optional immutable artifact version to remove.

- cache_dir:

  Transform cache directory, normally \[transform_cache_path()\].

## Value

The number of artifact files removed, invisibly.
