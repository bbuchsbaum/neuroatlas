# Fetch Exact Pinned Surface Registration Geometry

Downloads checksum-locked registration spheres, cortical masks and
vertex areas from their original providers. Only the pinned fsaverage
164k and fsLR 32k domains are supported. The fsLR sphere is in fsaverage
correspondence. Inputs retain their upstream licenses, independently of
neuroatlas's license; see \`extdata/surface-assets-LICENSES.md\` in the
installed package.

## Usage

``` r
get_surface_geometry(
  template,
  density,
  hemisphere,
  cache_dir = transform_cache_path(),
  download = TRUE,
  offline = FALSE
)
```

## Arguments

- template:

  Exact family, \`"fsaverage"\` or \`"fsLR"\`.

- density:

  \`"164k"\` for fsaverage or \`"32k"\` for fsLR.

- hemisphere:

  \`"L"\` or \`"R"\`.

- cache_dir:

  Dedicated transform cache directory.

- download:

  Allow downloads from pinned upstream URLs.

- offline:

  Use only checksum-verified local files.

## Value

A \`SurfaceGeometry\` with exact domain identity, pinned input hashes,
cortical mask, and upstream file provenance.

## Examples

``` r
if (FALSE) { # \dontrun{
left <- get_surface_geometry("fsaverage", "164k", "L")
target <- get_surface_geometry("fsLR", "32k", "L")
transform <- get_template_transform(left, target)
x <- surface_data(rep(0.25, left$domain$n_vertices), left$domain)
y <- apply_template_transform(x, transform)
} # }
```
