# Fetch a Template from TemplateFlow

Unified function to retrieve neuroimaging templates and related files
from the TemplateFlow repository. This function provides a more R-native
interface to the underlying Python `templateflow.api.get()` method.

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
  template file. Supported: `"T1w"` (default), `"T2w"`, `"mask"` (often
  used with `variant="mask"`). This is used to infer `suffix` if not
  explicitly provided.

- resolution:

  (Optional) Character string specifying the resolution, primarily for
  fsaverage variants (e.g., "06" for fsaverage6, which is
  \`tpl-fsaverage_res-06...\`). Passed as \`resolution\` to
  \`get_template\`.

- cohort:

  Character string. Optional cohort identifier (e.g., `"adhokshaj"`).

- desc:

  Character string. Specific TemplateFlow `desc` field. Defaults to
  `"brain"` for volumetric templates and is automatically dropped for
  variants where `desc` is typically unused (e.g., `probseg`, `dseg`,
  surface queries). If provided explicitly, this overrides any inferred
  `desc`.

- label:

  Character string. Specific TemplateFlow `label` field (e.g., `"GM"`,
  `"WM"`, `"CSF"` for `variant="probseg"` or `variant="dseg"`).

- atlas:

  Character string. Specific TemplateFlow `atlas` field (e.g.,
  `"Schaefer2018"`).

- suffix:

  Character string. Specific TemplateFlow `suffix` field. If provided,
  this overrides any `suffix` inferred from `modality` or `variant`.

- extension:

  Character string. The file extension. Default: `".nii.gz"`.

- path_only:

  Logical. If `TRUE`, returns the file path to the template as a string
  instead of loading it as a `NeuroVol` object. Default: `FALSE`.

- use_cache:

  Logical. (Currently primarily for future R-level memoisation).
  TemplateFlow's Python API has its own caching. Default: \`TRUE\`.
  Actual R-level path memoisation is now active.

- api_handle:

  An optional S3 object of class \`templateflow\` obtained from
  \`create_templateflow()\`. If \`NULL\` (default), a default instance
  is created internally.

- ...:

  Additional arguments passed directly to the Python
  \`templateflow.api.get()\` method (e.g., \`raise_on_empty = TRUE\`).
  This allows specifying any valid TemplateFlow query entity not
  explicitly listed as a parameter (e.g., \`hemi\`, \`density\`).

- template_id:

  The main TemplateFlow template identifier for the surface (e.g.,
  "fsLR", "fsaverage"). This is passed as the \`space\` argument to
  \`get_template\`.

- surface_type:

  A character string indicating the type of surface to retrieve. Common
  values include: "pial", "white", "inflated", "midthickness", "sphere".
  This is passed as the \`desc\` argument to \`get_template\`.

- hemi:

  Character string, "L" for left hemisphere or "R" for right hemisphere.
  Passed as \`hemi\` to \`get_template\`.

- density:

  (Optional) Character string specifying the surface density (e.g.,
  "32k" for fsLR, "164k" for fsaverage). Forwarded to TemplateFlow as
  \`density\`.

- load_as_path:

  Logical, whether to return only the path to the file. Defaults to
  \`TRUE\` as \`NeuroVol\` objects are not typically used for surface
  geometry. If \`FALSE\`, attempts to load using \`as_neurovol\` (via
  \`get_template\`).

## Value

If any of \`space\`, \`variant\`, \`modality\`, \`resolution\`, or
\`label\` are vectors of length \> 1 (and only one of them is vectorized
per call), a named list of results is returned. The names of the list
elements correspond to the values of the vectorized parameter. If all
parameters are scalar (or vectors of length 1), a single
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
object or a file path string is returned directly (depending on
`path_only`).

If \`load_as_path\` is \`TRUE\`, a character string (path) or a list of
character strings (paths). If \`load_as_path\` is \`FALSE\`, the result
of \`as_neurovol\` (which might be a \`NeuroVol\` if
\`neuroim2::read_vol\` supports the format, or could error if not).
Returns \`NULL\` if no template is found.

## Details

The function performs several pre-flight checks: - Validates the
existence of the specified \`space\` using \`tf\$api\$templates()\`. -
Validates the specified \`resolution\` against available resolutions
when metadata is available (legacy \`api\$resolutions()\` or
\`api\$get_metadata()\` fallback). The check is skipped silently if
resolution metadata cannot be retrieved.

Caching behavior: - This function uses \`memoise\` to cache the resolved
file paths from TemplateFlow at the R level for the current session. -
The underlying Python TemplateFlow library also maintains its own disk
cache, typically configured via the \`TEMPLATEFLOW_HOME\` environment
variable (which this package helps manage).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Ensure Python and templateflow module are available
  if (reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("templateflow")) {

    # Get default MNI T1w brain template (scalar call)
    mni_brain <- get_template()
    print(mni_brain)

    # Vectorized call: Get MNI brain and mask variants
    # mni_variants <- get_template(variant = c("brain", "mask"))
    # print(names(mni_variants))
    # print(mni_variants$brain)
    # print(mni_variants$mask)

    # Vectorized call: Get MNI T1w at 1mm and 2mm resolutions
    # mni_resolutions <- get_template(resolution = c(1, 2))
    # print(mni_resolutions$`1`)
    # print(mni_resolutions$`2`)

    # Vectorized call: Get GM and CSF probseg for MNI
    # mni_probsegs <- get_template(variant = "probseg", label = c("GM", "CSF"))
    # print(mni_probsegs$GM)

    # Path only example with vectorization
    # mni_mask_paths <- get_template(space = "MNI152NLin2009cAsym",
    #                              variant = "mask",
    #                              resolution = c(1,2),
    #                              path_only = TRUE)
    # print(mni_mask_paths)

  } else {
    message("Python or templateflow module not available. Skipping example.")
  }
} # }
# \donttest{
  # Get the pial surface for the left hemisphere of fsLR 32k template (as path)
  # fslr_pial_L_path <- get_surface_template(template_id = "fsLR", surface_type = "pial",
  #                                        hemi = "L", density = "32k")
  # print(fslr_pial_L_path)

  # Get the white surface for fsaverage6 (res="06", density="41k") right hemisphere
  # fsaverage6_white_R_path <- get_surface_template(template_id = "fsaverage",
  #                                               surface_type = "white",
  #                                               hemi = "R",
  #                                               resolution = "06", # for fsaverage6
  #                                               density = "41k")   # for fsaverage6
  # print(fsaverage6_white_R_path)
# }
```
