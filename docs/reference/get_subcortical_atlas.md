# Load harmonized subcortical atlases via TemplateFlow

Fetches one of the AtlasPack-derived subcortical atlases (CIT168, HCP
thalamus, MDTB10 cerebellum, or HCP hippocampus/amygdala) using the
existing TemplateFlow integration. The returned object includes the
source template space and resolution so downstream workflows can track
provenance.

## Usage

``` r
get_subcortical_atlas(
  name,
  template_space = NULL,
  resolution = NULL,
  desc = NULL,
  outspace = NULL,
  use_cache = TRUE,
  path_only = FALSE,
  ...
)
```

## Arguments

- name:

  Atlas identifier; see
  [`subcortical_atlas_options()`](subcortical_atlas_options.md). Aliases
  such as "thalamus_hcp" or "cerebellum" are accepted.

- template_space:

  TemplateFlow space to download (e.g., `"MNI152NLin6Asym"` or
  `"MNI152NLin2009cAsym"`). Defaults to the atlas-specific recommended
  space.

- resolution:

  TemplateFlow resolution (e.g., "01"). Defaults to the atlas
  recommendation; validated against known options for the atlas.

- desc:

  Optional TemplateFlow `desc` to override the atlas default (e.g.,
  "LRSplit" for CIT168 hemispheric split).

- outspace:

  Optional `NeuroSpace` or TemplateFlow query (string/list) to resample
  the atlas into a different space.

- use_cache:

  Logical; pass through to [`get_template`](get_template.md).

- path_only:

  Logical; if TRUE, return paths to the atlas/label files plus metadata
  instead of loading into R objects.

- ...:

  Additional arguments forwarded to [`get_template`](get_template.md).

## Value

When `path_only=FALSE` (default), a list with classes
`c("subcortical", "atlas")` containing:

- `name`: human-friendly atlas name

- `atlas`: `ClusteredNeuroVol` with parcellation labels

- `ids`: integer vector of region IDs

- `labels`: character vector of region labels

- `orig_labels`: same as `labels`

- `hemi`: inferred hemisphere labels when available

- `cmap`: optional RGB mapping if provided by the label file

- `space`, `resolution`, `desc`: TemplateFlow metadata

- `template_atlas`: TemplateFlow `atlas` parameter used

When `path_only=TRUE`, a list with class `c("subcortical_paths","list")`
containing `atlas_path`, `label_path`, and the same metadata fields
above.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load CIT168 in NLin6Asym space
cit <- get_subcortical_atlas("cit168", template_space = "MNI152NLin6Asym")

# Get only the file paths without loading volumes
paths <- get_subcortical_atlas("mdtb10", path_only = TRUE)
} # }
```
