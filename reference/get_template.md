# Fetch a Template from TemplateFlow

Unified function to retrieve neuroimaging templates and related files
from the TemplateFlow repository via the pure R `templateflow` package.

## Usage

``` r
get_template(
  space = "MNI152NLin2009cAsym",
  variant = "brain",
  modality = "T1w",
  resolution = 1,
  cohort = NULL,
  desc = "brain",
  label = NULL,
  atlas = NULL,
  suffix = NULL,
  extension = ".nii.gz",
  path_only = FALSE,
  use_cache = TRUE,
  api_handle = NULL,
  ...
)

get_surface_template(
  template_id,
  surface_type,
  hemi,
  density = NULL,
  resolution = NULL,
  ...,
  load_as_path = TRUE
)
```

## Arguments

- space:

  Character string. The primary TemplateFlow identifier for the template
  space (e.g., `"MNI152NLin2009cAsym"`). Default:
  `"MNI152NLin2009cAsym"`.

- variant:

  Character string. A high-level descriptor for common template types.
  Supported: `"brain"` (default), `"head"`, `"mask"`, `"probseg"`,
  `"dseg"`. This is used to infer `desc` and sometimes `suffix` if they
  are not explicitly provided.

- modality:

  Character string. The imaging modality or primary suffix for the
  template file. Supported: `"T1w"` (default), `"T2w"`, `"mask"`. This
  is used to infer `suffix` if not explicitly provided.

- resolution:

  (Optional) Character string specifying the resolution, primarily for
  fsaverage variants (e.g., "06" for fsaverage6).

- cohort:

  Character string. Optional cohort identifier.

- desc:

  Character string. Specific TemplateFlow `desc` field. Defaults to
  `"brain"`.

- label:

  Character string. Specific TemplateFlow `label` field (e.g., `"GM"`,
  `"WM"`, `"CSF"`).

- atlas:

  Character string. Specific TemplateFlow `atlas` field (e.g.,
  `"Schaefer2018"`).

- suffix:

  Character string. Specific TemplateFlow `suffix` field. Overrides any
  `suffix` inferred from `modality` or `variant`.

- extension:

  Character string. The file extension. Default: `".nii.gz"`.

- path_only:

  Logical. If `TRUE`, returns the file path as a string instead of
  loading as a `NeuroVol`. Default: `FALSE`.

- use_cache:

  Logical. If `TRUE` (default), uses memoised NeuroVol loading.

- api_handle:

  Deprecated and ignored. Kept for backward compatibility.

- ...:

  Additional arguments passed to the TemplateFlow query (e.g., `hemi`,
  `density`).

- template_id:

  The main TemplateFlow template identifier for the surface (e.g.,
  "fsLR", "fsaverage"). This is passed as the \`space\` argument to
  \`get_template\`.

- surface_type:

  A character string indicating the type of surface to retrieve. Common
  values include: "pial", "white", "inflated", "midthickness", "sphere".
  This is passed as the \`suffix\` argument to \`get_template\`.

- hemi:

  Character string, "L" for left hemisphere or "R" for right hemisphere.
  Passed as \`hemi\` to \`get_template\`.

- density:

  (Optional) Character string specifying the surface density (e.g.,
  "32k" for fsLR, "164k" for fsaverage). Forwarded to TemplateFlow as
  \`density\`.

- load_as_path:

  Logical, whether to return only the path to the file. Defaults to
  \`TRUE\`.

## Value

If any of `space`, `variant`, `modality`, `resolution`, or `label` are
vectors of length \> 1, a named list of results is returned. Otherwise a
single
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
or file path string.

If \`load_as_path\` is \`TRUE\`, a character string (path). If
\`load_as_path\` is \`FALSE\`, the result of \`as_neurovol\`.

## Details

The function performs several pre-flight checks: - Validates the
existence of the specified `space`. - Validates the specified
`resolution` against available resolutions.

Caching behaviour: - The `templateflow` R package maintains its own disk
cache. - `NeuroVol` loading is memoised at the R session level to avoid
re-reading large NIfTI files.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Get default MNI T1w brain template
  mni_brain <- get_template()

  # Vectorized: Get MNI brain and mask variants
  mni_variants <- get_template(variant = c("brain", "mask"))

  # Path only
  path <- get_template(path_only = TRUE)
} # }
# \donttest{
  # Get the pial surface for the left hemisphere of fsLR 32k template (as path)
  # fslr_pial_L_path <- get_surface_template(template_id = "fsLR", surface_type = "pial",
  #                                        hemi = "L", density = "32k")
  # print(fslr_pial_L_path)
# }
```
