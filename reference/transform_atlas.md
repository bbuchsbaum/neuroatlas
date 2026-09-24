# Transform a Volumetric Atlas to Another Template

Uses atlas metadata to resolve the source space, then applies a verified
template transform on the requested target grid. Region identities and
source receipts are retained. This changes spatial coordinates, not the
atlas's parcellation scheme; use \[atlas_overlap()\] after alignment to
compare parcels.

## Usage

``` r
transform_atlas(
  x,
  to_space,
  target = NULL,
  resolution = NULL,
  provider = "auto",
  ...
)
```

## Arguments

- x:

  A volumetric atlas with a declared template identity.

- to_space:

  Exact target template identifier.

- target:

  Explicit target atlas, volume or grid. If \`NULL\`, load the requested
  template through \[get_template()\].

- resolution:

  Template resolution, required when \`target\` is \`NULL\`.

- provider:

  Artifact provider.

- ...:

  Download/cache options passed to \[get_template_transform()\].

## Value

A transformed atlas with updated spatial metadata and history.
