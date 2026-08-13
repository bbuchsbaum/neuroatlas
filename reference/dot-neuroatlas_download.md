# Download an Atlas Asset (hard).

Download an Atlas Asset (hard).

## Usage

``` r
.neuroatlas_download(
  url,
  dest = NULL,
  mode = "wb",
  quiet = TRUE,
  min_size = 1024L,
  description = "atlas asset"
)
```

## Arguments

- url:

  URL to fetch.

- dest:

  Destination path. If \`NULL\`, a path in \[tempdir()\] based on
  \`basename(url)\` is used.

- mode:

  Passed through to \[downloader::download()\] (default \`"wb"\`).

- quiet:

  Passed through to \[downloader::download()\].

- min_size:

  Minimum acceptable file size in bytes. Anything smaller is treated as
  a failed/corrupt download (typical LFS pointer stubs are well under
  this threshold).

- description:

  Short human-readable description of the asset, used in error messages
  (e.g. \`"Schaefer volume"\`).

## Value

The destination path (invisibly on success).
