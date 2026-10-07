# Transform a Volumetric Atlas to a Template or Cortical Surface

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

  Exact target template identifier or verified \`SurfaceGeometry\`.

- target:

  Explicit target atlas, volume or grid. If \`NULL\`, load the requested
  template through \[get_template()\]. A \`SurfaceGeometry\` supplies an
  exact cortical destination, matching \`to_space\` when both are
  supplied.

- resolution:

  Template resolution, required when \`target\` is \`NULL\`.

- provider:

  Artifact provider.

- ...:

  Download/cache options passed to \[get_template_transform()\].

## Value

For a volume destination, a transformed atlas with updated spatial
metadata and history. For a cortical destination, \`SurfaceData\` with
the original region IDs and names, source atlas reference, coverage and
lost-label reporting. Population projection does not establish subject
registration.

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_harvard_oxford_atlas("cortical", resolution = "02")
transformed <- transform_atlas(atlas, "MNI152NLin2009cAsym", resolution = 2)
} # }
```
