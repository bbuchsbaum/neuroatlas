# Resolve and Load a Verified Template Transform

Resolves an available image-space route and loads its verified
artifacts. Planning and fitting are separate: this function never
estimates a registration. Nonlinear routes must have a published
checksum and a passing qualification record in
\[space_transform_manifest()\].

## Usage

``` r
get_template_transform(
  from,
  to,
  provider = c("auto", "neuroatlas", "templateflow"),
  download = TRUE,
  verify = TRUE,
  cache_dir = transform_cache_path(),
  offline = FALSE
)
```

## Arguments

- from, to:

  Exact source and target template identifiers.

- provider:

  Artifact provider, or \`"auto"\` for registry selection.

- download:

  Allow missing artifacts to be downloaded.

- verify:

  Must be \`TRUE\`; artifact integrity cannot be disabled.

- cache_dir:

  Dedicated transform cache directory.

- offline:

  Use only verified local artifacts.

## Value

A \`template_transform\` containing the plan, files, pullback morphism,
and artifact provenance. Its direction describes image movement; its
morphism maps target coordinates into source coordinates for sampling.

## See also

\[apply_template_transform()\], \[transform_atlas()\]
