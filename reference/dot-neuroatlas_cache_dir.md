# Get or Create neuroatlas Cache Directory

Returns a path to a neuroatlas-specific cache directory. If the
directory (or a specified subdirectory) doesn't exist, it will be
created. This function uses \`tools::R_user_dir\` to ensure a
user-specific, OS-appropriate cache location.

## Usage

``` r
.neuroatlas_cache_dir(subdir = NULL)
```

## Arguments

- subdir:

  Optional character string. If provided, a subdirectory named
  \`subdir\` will be created/used within the main neuroatlas cache
  directory.

## Value

A character string representing the path to the cache directory.
