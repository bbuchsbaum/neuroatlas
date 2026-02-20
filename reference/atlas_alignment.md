# Atlas Alignment Lookup

Returns a structured compatibility/alignment summary between two atlas
representations based on atlas provenance metadata.

## Usage

``` r
atlas_alignment(x, y)
```

## Arguments

- x:

  Source atlas object.

- y:

  Target atlas object.

## Value

A list of class \`"atlas_alignment"\` with fields: \`from\`, \`to\`,
\`compatible\`, \`relation\`, \`method\`, \`confidence\`, \`status\`,
\`requires_transform\`, and \`notes\`.

## Details

For same family/model/representation comparisons across templates, this
function consults the space transform registry when available.

## Examples

``` r
ref <- new_atlas_ref(
  family = "schaefer",
  model = "Schaefer2018",
  representation = "volume",
  template_space = "MNI152NLin6Asym",
  coord_space = "MNI152",
  confidence = "high"
)
a <- structure(list(atlas_ref = ref), class = c("schaefer", "atlas"))
b <- structure(list(atlas_ref = ref), class = c("schaefer", "atlas"))
atlas_alignment(a, b)$relation
#> [1] "identical"
```
