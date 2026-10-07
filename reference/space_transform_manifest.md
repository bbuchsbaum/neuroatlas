# Space Transform Manifest

Returns known transforms between coordinate/template spaces from a
static registry shipped with the package.

## Usage

``` r
space_transform_manifest(status = NULL)
```

## Arguments

- status:

  Optional character vector to filter by status (e.g., \`"available"\`,
  \`"planned"\`).

## Value

A data frame with one row per transform route and columns:
\`from_space\`, \`to_space\`, \`transform_type\`, \`backend\`,
\`confidence\`, \`reversible\`, \`data_files\`, \`status\`, and
\`notes\`. Artifact-backed routes also record \`artifact_id\`,
\`artifact_version\`, \`provider\`, \`url\`, \`sha256\`, \`size_bytes\`,
\`format\`, \`convention\`, \`qualification\`, \`qualification_scope\`,
\`qa_url\`, and \`license\`. These fields are missing for routes without
a downloadable artifact. \`source_representation\` and
\`target_representation\` distinguish volumes and surfaces.
\`executable\` indicates support by the template-transform API, given
the required optional dependencies and artifacts; it is not a local
readiness check. Exact admitted surface routes also bind
\`from_domain_id\`, \`to_domain_id\`, \`method\`, \`engine_revision\`,
\`input_lock_sha256\` and hemisphere. \`from_density\` and
\`to_density\` record pinned surface densities; \`route_scope\`
distinguishes \`"roadmap_placeholder"\`, \`"exact_surface_domains"\`,
\`"exact_volume_to_surface"\`, \`"exact_template_pair"\`, and legacy
\`"coordinate_family"\` routes. Broad surface names remain advisory.
Numerical qualification is restricted to the recorded inputs and
methods; it does not establish anatomical accuracy.

## Examples

``` r
# All known routes
reg <- space_transform_manifest()

# Executable routes, with their identity scope
reg[reg$executable, c("from_space", "to_space", "route_scope",
  "hemisphere", "from_density", "to_density", "backend")]
#>             from_space            to_space             route_scope hemisphere
#> 1               MNI305              MNI152       coordinate_family       <NA>
#> 2               MNI152              MNI305       coordinate_family       <NA>
#> 3      MNI152NLin6Asym MNI152NLin2009cAsym     exact_template_pair       <NA>
#> 4  MNI152NLin2009cAsym     MNI152NLin6Asym     exact_template_pair       <NA>
#> 13           fsaverage            fsLR_32k   exact_surface_domains          L
#> 14            fsLR_32k           fsaverage   exact_surface_domains          L
#> 15           fsaverage            fsLR_32k   exact_surface_domains          R
#> 16            fsLR_32k           fsaverage   exact_surface_domains          R
#> 17     MNI152NLin6Asym           fsaverage exact_volume_to_surface          L
#> 18     MNI152NLin6Asym            fsLR_32k exact_volume_to_surface          L
#> 19     MNI152NLin6Asym           fsaverage exact_volume_to_surface          R
#> 20     MNI152NLin6Asym            fsLR_32k exact_volume_to_surface          R
#> 21 MNI152NLin2009cAsym           fsaverage exact_volume_to_surface          L
#> 22 MNI152NLin2009cAsym            fsLR_32k exact_volume_to_surface          L
#> 23 MNI152NLin2009cAsym           fsaverage exact_volume_to_surface          R
#> 24 MNI152NLin2009cAsym            fsLR_32k exact_volume_to_surface          R
#> 25           fsaverage          fsaverage6   exact_surface_domains          L
#> 26          fsaverage6           fsaverage   exact_surface_domains          L
#> 27           fsaverage          fsaverage5   exact_surface_domains          L
#> 28          fsaverage5           fsaverage   exact_surface_domains          L
#> 29           fsaverage          fsaverage6   exact_surface_domains          R
#> 30          fsaverage6           fsaverage   exact_surface_domains          R
#> 31           fsaverage          fsaverage5   exact_surface_domains          R
#> 32          fsaverage5           fsaverage   exact_surface_domains          R
#>    from_density to_density                  backend
#> 1          <NA>       <NA>          internal_affine
#> 2          <NA>       <NA>          internal_affine
#> 3          <NA>       <NA>           neurotransform
#> 4          <NA>       <NA>           neurotransform
#> 13         164k        32k    neurotransform_native
#> 14          32k       164k    neurotransform_native
#> 15         164k        32k    neurotransform_native
#> 16          32k       164k    neurotransform_native
#> 17         <NA>       164k cbig_registration_fusion
#> 18         <NA>        32k cbig_registration_fusion
#> 19         <NA>       164k cbig_registration_fusion
#> 20         <NA>        32k cbig_registration_fusion
#> 21         <NA>       164k cbig_registration_fusion
#> 22         <NA>        32k cbig_registration_fusion
#> 23         <NA>       164k cbig_registration_fusion
#> 24         <NA>        32k cbig_registration_fusion
#> 25         164k        41k    neurotransform_native
#> 26          41k       164k    neurotransform_native
#> 27         164k        10k    neurotransform_native
#> 28          10k       164k    neurotransform_native
#> 29         164k        41k    neurotransform_native
#> 30          41k       164k    neurotransform_native
#> 31         164k        10k    neurotransform_native
#> 32          10k       164k    neurotransform_native
```
