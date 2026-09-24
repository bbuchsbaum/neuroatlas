# Atlas Transform Manifest

Returns currently known cross-representation alignment routes and their
implementation status.

## Usage

``` r
atlas_transform_manifest(scope = c("alignment", "space"))
```

## Arguments

- scope:

  Manifest scope. \`"alignment"\` returns family/model alignment routes.
  \`"space"\` returns template/space transform routes.

## Value

A data frame manifest. For \`scope = "alignment"\` this is a tibble of
atlas-family representation routes. For \`scope = "space"\` this is the
space transform registry returned by \[space_transform_manifest()\].

## Examples

``` r
# Atlas-family alignment routes
atlas_transform_manifest("alignment")
#> # A tibble: 6 × 9
#>   family  model from_representation to_representation relation method confidence
#>   <chr>   <chr> <chr>               <chr>             <chr>    <chr>  <chr>     
#> 1 schaef… Scha… volume              surface           same_mo… volum… approxima…
#> 2 schaef… Scha… surface             volume            same_mo… surfa… approxima…
#> 3 glasser HCP-… volume              surface           same_mo… volum… uncertain 
#> 4 glasser HCP-… surface             volume            same_mo… surfa… uncertain 
#> 5 subcor… CIT1… volume              volume            same_mo… resam… approxima…
#> 6 subcor… CIT1… volume              volume            same_mo… resam… approxima…
#> # ℹ 2 more variables: status <chr>, notes <chr>

# Space-to-space routes
atlas_transform_manifest("space")
#>             from_space            to_space  transform_type         backend
#> 1               MNI305              MNI152          affine internal_affine
#> 2               MNI152              MNI305          affine internal_affine
#> 3      MNI152NLin6Asym MNI152NLin2009cAsym  nonlinear_warp  neurotransform
#> 4  MNI152NLin2009cAsym     MNI152NLin6Asym  nonlinear_warp  neurotransform
#> 5            fsaverage          fsaverage6 sphere_resample       sphere_nn
#> 6           fsaverage6           fsaverage sphere_resample       sphere_nn
#> 7            fsaverage          fsaverage5 sphere_resample       sphere_nn
#> 8           fsaverage5           fsaverage sphere_resample       sphere_nn
#> 9            fsaverage            fsLR_32k sphere_resample       workbench
#> 10            fsLR_32k           fsaverage sphere_resample       workbench
#> 11 MNI152NLin2009cAsym           fsaverage        vol2surf       neurosurf
#> 12           fsaverage MNI152NLin2009cAsym        surf2vol     ribbon_fill
#>     confidence reversible
#> 1        exact       TRUE
#> 2        exact       TRUE
#> 3         high       TRUE
#> 4         high       TRUE
#> 5        exact       TRUE
#> 6        exact       TRUE
#> 7        exact       TRUE
#> 8        exact       TRUE
#> 9         high       TRUE
#> 10        high       TRUE
#> 11 approximate      FALSE
#> 12 approximate      FALSE
#>                                                        data_files    status
#> 1                                                            <NA> available
#> 2                                                            <NA> available
#> 3  tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5 available
#> 4  tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5 available
#> 5                                    fsaverage/surf/?h.sphere.reg available
#> 6                                    fsaverage/surf/?h.sphere.reg available
#> 7                                    fsaverage/surf/?h.sphere.reg available
#> 8                                    fsaverage/surf/?h.sphere.reg available
#> 9              fs_LR-deformed_to-fsaverage.?H.sphere.reg.surf.gii   planned
#> 10                      fsaverage_to-fs_LR.?H.sphere.reg.surf.gii   planned
#> 11                                                           <NA>   planned
#> 12                                                           <NA>   planned
#>                                                            notes
#> 1                                 FreeSurfer mni152.register.dat
#> 2                      Inverse of FreeSurfer mni152.register.dat
#> 3  neuroatlas ANTs pair; qualified on 1 mm and 2 mm target grids
#> 4  neuroatlas ANTs pair; qualified on 1 mm and 2 mm target grids
#> 5                  Downsample 164k to 41k on registration sphere
#> 6                    Upsample 41k to 164k on registration sphere
#> 7                  Downsample 164k to 10k on registration sphere
#> 8                    Upsample 10k to 164k on registration sphere
#> 9                          HCP sphere registration via Workbench
#> 10                         HCP sphere registration via Workbench
#> 11                              Requires white and pial surfaces
#> 12                             Requires ribbon mask construction
#>                                                    artifact_id
#> 1                                                         <NA>
#> 2                                                         <NA>
#> 3  tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm
#> 4  tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm
#> 5                                                         <NA>
#> 6                                                         <NA>
#> 7                                                         <NA>
#> 8                                                         <NA>
#> 9                                                         <NA>
#> 10                                                        <NA>
#> 11                                                        <NA>
#> 12                                                        <NA>
#>          artifact_version   provider
#> 1                    <NA>       <NA>
#> 2                    <NA>       <NA>
#> 3  transform-artifacts-v1 neuroatlas
#> 4  transform-artifacts-v1 neuroatlas
#> 5                    <NA>       <NA>
#> 6                    <NA>       <NA>
#> 7                    <NA>       <NA>
#> 8                    <NA>       <NA>
#> 9                    <NA>       <NA>
#> 10                   <NA>       <NA>
#> 11                   <NA>       <NA>
#> 12                   <NA>       <NA>
#>                                                                                                                                                 url
#> 1                                                                                                                                              <NA>
#> 2                                                                                                                                              <NA>
#> 3  https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5
#> 4  https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5
#> 5                                                                                                                                              <NA>
#> 6                                                                                                                                              <NA>
#> 7                                                                                                                                              <NA>
#> 8                                                                                                                                              <NA>
#> 9                                                                                                                                              <NA>
#> 10                                                                                                                                             <NA>
#> 11                                                                                                                                             <NA>
#> 12                                                                                                                                             <NA>
#>                                                              sha256 size_bytes
#> 1                                                              <NA>         NA
#> 2                                                              <NA>         NA
#> 3  8275d687c4230f56fcc45d7c6ac603e0f8b6685ae95beeeae9434df3e552909c   89954288
#> 4  ac0bf960d3674c13d24450aa43573cfc3605dc5ac251635c2b345dd4d446246d   89958649
#> 5                                                              <NA>         NA
#> 6                                                              <NA>         NA
#> 7                                                              <NA>         NA
#> 8                                                              <NA>         NA
#> 9                                                              <NA>         NA
#> 10                                                             <NA>         NA
#> 11                                                             <NA>         NA
#> 12                                                             <NA>         NA
#>     format              convention qualification   qualification_scope
#> 1     <NA>                    <NA>          <NA>                  <NA>
#> 2     <NA>                    <NA>          <NA>                  <NA>
#> 3  ants_h5 ants_image_pullback_ras        passed template_pair_1mm_2mm
#> 4  ants_h5 ants_image_pullback_ras        passed template_pair_1mm_2mm
#> 5     <NA>                    <NA>          <NA>                  <NA>
#> 6     <NA>                    <NA>          <NA>                  <NA>
#> 7     <NA>                    <NA>          <NA>                  <NA>
#> 8     <NA>                    <NA>          <NA>                  <NA>
#> 9     <NA>                    <NA>          <NA>                  <NA>
#> 10    <NA>                    <NA>          <NA>                  <NA>
#> 11    <NA>                    <NA>          <NA>                  <NA>
#> 12    <NA>                    <NA>          <NA>                  <NA>
#>                                                                                       qa_url
#> 1                                                                                       <NA>
#> 2                                                                                       <NA>
#> 3  https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/qa.json
#> 4  https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/qa.json
#> 5                                                                                       <NA>
#> 6                                                                                       <NA>
#> 7                                                                                       <NA>
#> 8                                                                                       <NA>
#> 9                                                                                       <NA>
#> 10                                                                                      <NA>
#> 11                                                                                      <NA>
#> 12                                                                                      <NA>
#>                                                                                          license
#> 1                                                                                           <NA>
#> 2                                                                                           <NA>
#> 3  https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/LICENSES.md
#> 4  https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/LICENSES.md
#> 5                                                                                           <NA>
#> 6                                                                                           <NA>
#> 7                                                                                           <NA>
#> 8                                                                                           <NA>
#> 9                                                                                           <NA>
#> 10                                                                                          <NA>
#> 11                                                                                          <NA>
#> 12                                                                                          <NA>
```
