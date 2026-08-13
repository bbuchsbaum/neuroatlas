# Load a Harvard-Oxford Atlas

Loads Harvard-Oxford cortical, subcortical, or combined
cortical/subcortical parcellations. By default this uses TemplateFlow,
which does not require a local FSL installation. Set \`source = "fsl"\`
to read from \`\$FSLDIR\`.

## Usage

``` r
get_harvard_oxford_atlas(
  type = c("cortical", "subcortical", "cortical_subcortical"),
  threshold = c(25, 0, 50),
  template_space = "MNI152NLin6Asym",
  resolution = "01",
  source = c("templateflow", "fsl"),
  outspace = NULL,
  use_cache = TRUE,
  path_only = FALSE
)

get_harvard_oxford_cortical_atlas(...)

get_harvard_oxford_subcortical_atlas(...)

get_harvard_oxford_cortical_subcortical_atlas(...)
```

## Arguments

- type:

  One of \`"cortical"\`, \`"subcortical"\`, or
  \`"cortical_subcortical"\`.

- threshold:

  Maximum-probability threshold, one of \`0\`, \`25\`, or \`50\`.

- template_space:

  TemplateFlow space.

- resolution:

  TemplateFlow/FSL resolution. TemplateFlow accepts \`"01"\` or
  \`"02"\`; FSL accepts values such as \`"1mm"\` and \`"2mm"\`.

- source:

  \`"templateflow"\` or \`"fsl"\`.

- outspace:

  Optional \`NeuroSpace\` to resample the atlas into.

- use_cache:

  Passed through to \`get_template()\`.

- path_only:

  Return resolved paths and metadata without loading image data.

- download:

  Logical; for \`get_julich_brain_atlas()\`, download the Julich-Brain
  atlas archive into the neuroatlas cache when \`fsl_dir\` is unset.

## Value

An \`atlas\` object, or path metadata when \`path_only = TRUE\`.
