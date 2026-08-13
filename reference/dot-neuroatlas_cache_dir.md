# Get or Create neuroatlas Cache Directory

Returns a path to a neuroatlas-specific cache directory. If the
directory (or a specified subdirectory) doesn't exist, it will be
created. This function uses \`tools::R_user_dir\` to ensure a
user-specific, OS-appropriate cache location.

## Usage

``` r
.neuroatlas_cache_dir(subdir = NULL, create = TRUE)
```

## Arguments

- subdir:

  Optional character string. If provided, a subdirectory named
  \`subdir\` will be created/used within the main neuroatlas cache
  directory.

- create:

  Logical. If \`TRUE\` (default), create the directory if it does not
  exist. If \`FALSE\`, return the path without creating it (for
  read-only lookups that must not write to the user's home cache).

## Value

A character string representing the path to the cache directory.
