# Download an Atlas Asset (soft).

Wraps \[.neuroatlas_download()\] but returns a status list instead of
throwing, so callers can implement fallback strategies while still
having access to the full error condition (rather than a silent
\`NULL\`).

## Usage

``` r
.neuroatlas_try_download(
  url,
  dest = NULL,
  mode = "wb",
  quiet = TRUE,
  min_size = 1024L,
  description = "atlas asset"
)
```

## Value

A list with fields: \* \`ok\`: \`TRUE\` on success. \* \`path\`: path to
the downloaded file (or \`NULL\`). \* \`error\`: the captured error
condition on failure (or \`NULL\`). \* \`lfs_pointer\`: \`TRUE\` if the
file looked like a Git LFS/git-annex pointer stub.
