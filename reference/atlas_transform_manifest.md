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
#>             from_space            to_space  transform_type
#> 1               MNI305              MNI152          affine
#> 2               MNI152              MNI305          affine
#> 3      MNI152NLin6Asym MNI152NLin2009cAsym  nonlinear_warp
#> 4  MNI152NLin2009cAsym     MNI152NLin6Asym  nonlinear_warp
#> 5            fsaverage          fsaverage6 sphere_resample
#> 6           fsaverage6           fsaverage sphere_resample
#> 7            fsaverage          fsaverage5 sphere_resample
#> 8           fsaverage5           fsaverage sphere_resample
#> 9            fsaverage            fsLR_32k sphere_resample
#> 10            fsLR_32k           fsaverage sphere_resample
#> 11 MNI152NLin2009cAsym           fsaverage        vol2surf
#> 12           fsaverage MNI152NLin2009cAsym        surf2vol
#> 13           fsaverage            fsLR_32k sphere_resample
#> 14            fsLR_32k           fsaverage sphere_resample
#> 15           fsaverage            fsLR_32k sphere_resample
#> 16            fsLR_32k           fsaverage sphere_resample
#> 17     MNI152NLin6Asym           fsaverage        vol2surf
#> 18     MNI152NLin6Asym            fsLR_32k        vol2surf
#> 19     MNI152NLin6Asym           fsaverage        vol2surf
#> 20     MNI152NLin6Asym            fsLR_32k        vol2surf
#> 21 MNI152NLin2009cAsym           fsaverage        vol2surf
#> 22 MNI152NLin2009cAsym            fsLR_32k        vol2surf
#> 23 MNI152NLin2009cAsym           fsaverage        vol2surf
#> 24 MNI152NLin2009cAsym            fsLR_32k        vol2surf
#>                     backend  confidence reversible
#> 1           internal_affine approximate       TRUE
#> 2           internal_affine approximate       TRUE
#> 3            neurotransform        high       TRUE
#> 4            neurotransform        high       TRUE
#> 5                 sphere_nn approximate      FALSE
#> 6                 sphere_nn approximate      FALSE
#> 7                 sphere_nn approximate      FALSE
#> 8                 sphere_nn approximate      FALSE
#> 9                 workbench approximate      FALSE
#> 10                workbench approximate      FALSE
#> 11                neurosurf approximate      FALSE
#> 12              ribbon_fill approximate      FALSE
#> 13    neurotransform_native approximate      FALSE
#> 14    neurotransform_native approximate      FALSE
#> 15    neurotransform_native approximate      FALSE
#> 16    neurotransform_native approximate      FALSE
#> 17 cbig_registration_fusion approximate      FALSE
#> 18 cbig_registration_fusion approximate      FALSE
#> 19 cbig_registration_fusion approximate      FALSE
#> 20 cbig_registration_fusion approximate      FALSE
#> 21 cbig_registration_fusion approximate      FALSE
#> 22 cbig_registration_fusion approximate      FALSE
#> 23 cbig_registration_fusion approximate      FALSE
#> 24 cbig_registration_fusion approximate      FALSE
#>                                                                          data_files
#> 1                                                                              <NA>
#> 2                                                                              <NA>
#> 3                    tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5
#> 4                    tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5
#> 5                                                      fsaverage/surf/?h.sphere.reg
#> 6                                                      fsaverage/surf/?h.sphere.reg
#> 7                                                      fsaverage/surf/?h.sphere.reg
#> 8                                                      fsaverage/surf/?h.sphere.reg
#> 9                                fs_LR-deformed_to-fsaverage.?H.sphere.reg.surf.gii
#> 10                                        fsaverage_to-fs_LR.?H.sphere.reg.surf.gii
#> 11                                                                             <NA>
#> 12                                                                             <NA>
#> 13                                                                             <NA>
#> 14                                                                             <NA>
#> 15                                                                             <NA>
#> 16                                                                             <NA>
#> 17 CBIG RF-ANTs MNI152 original ras; explicit qualified pullback/native composition
#> 18 CBIG RF-ANTs MNI152 original ras; explicit qualified pullback/native composition
#> 19 CBIG RF-ANTs MNI152 original ras; explicit qualified pullback/native composition
#> 20 CBIG RF-ANTs MNI152 original ras; explicit qualified pullback/native composition
#> 21 CBIG RF-ANTs MNI152 original ras; explicit qualified pullback/native composition
#> 22 CBIG RF-ANTs MNI152 original ras; explicit qualified pullback/native composition
#> 23 CBIG RF-ANTs MNI152 original ras; explicit qualified pullback/native composition
#> 24 CBIG RF-ANTs MNI152 original ras; explicit qualified pullback/native composition
#>       status
#> 1  available
#> 2  available
#> 3  available
#> 4  available
#> 5    planned
#> 6    planned
#> 7    planned
#> 8    planned
#> 9    planned
#> 10   planned
#> 11   planned
#> 12   planned
#> 13 available
#> 14 available
#> 15 available
#> 16 available
#> 17 available
#> 18 available
#> 19 available
#> 20 available
#> 21 available
#> 22 available
#> 23 available
#> 24 available
#>                                                                                                                                                notes
#> 1                            Legacy FreeSurfer coordinate-family affine; generic MNI152 does not identify an exact template or cortical registration
#> 2                            Legacy FreeSurfer coordinate-family affine; generic MNI152 does not identify an exact template or cortical registration
#> 3                                                                                      neuroatlas ANTs pair; qualified on 1 mm and 2 mm target grids
#> 4                                                                                      neuroatlas ANTs pair; qualified on 1 mm and 2 mm target grids
#> 5                                                                                                      Downsample 164k to 41k on registration sphere
#> 6                                                                                                        Upsample 41k to 164k on registration sphere
#> 7                                                                                                      Downsample 164k to 10k on registration sphere
#> 8                                                                                                        Upsample 10k to 164k on registration sphere
#> 9                                                                                                              HCP sphere registration via Workbench
#> 10                                                                                                             HCP sphere registration via Workbench
#> 11                                                          Unqualified projection; requires exact volume identity and an explicit projection method
#> 12                                                                     Directed rasterization; not an inverse; requires coverage and conflict policy
#> 13                                       Exact pinned domains only; native geometric qualification, no Workbench parity or anatomical accuracy claim
#> 14                                       Exact pinned domains only; native geometric qualification, no Workbench parity or anatomical accuracy claim
#> 15                                       Exact pinned domains only; native geometric qualification, no Workbench parity or anatomical accuracy claim
#> 16                                       Exact pinned domains only; native geometric qualification, no Workbench parity or anatomical accuracy claim
#> 17 Exact-domain population sampling, tested on 1mm and 2mm source grids; numerical agreement does not establish anatomical accuracy or reversibility
#> 18 Exact-domain population sampling, tested on 1mm and 2mm source grids; numerical agreement does not establish anatomical accuracy or reversibility
#> 19 Exact-domain population sampling, tested on 1mm and 2mm source grids; numerical agreement does not establish anatomical accuracy or reversibility
#> 20 Exact-domain population sampling, tested on 1mm and 2mm source grids; numerical agreement does not establish anatomical accuracy or reversibility
#> 21 Exact-domain population sampling, tested on 1mm and 2mm source grids; numerical agreement does not establish anatomical accuracy or reversibility
#> 22 Exact-domain population sampling, tested on 1mm and 2mm source grids; numerical agreement does not establish anatomical accuracy or reversibility
#> 23 Exact-domain population sampling, tested on 1mm and 2mm source grids; numerical agreement does not establish anatomical accuracy or reversibility
#> 24 Exact-domain population sampling, tested on 1mm and 2mm source grids; numerical agreement does not establish anatomical accuracy or reversibility
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
#> 13                                               native_L_down
#> 14                                                 native_L_up
#> 15                                               native_R_down
#> 16                                                 native_R_up
#> 17                                       cbig_mni6_fsaverage_L
#> 18                                            cbig_mni6_fsLR_L
#> 19                                       cbig_mni6_fsaverage_R
#> 20                                            cbig_mni6_fsLR_R
#> 21                                   cbig_mni2009c_fsaverage_L
#> 22                                        cbig_mni2009c_fsLR_L
#> 23                                   cbig_mni2009c_fsaverage_R
#> 24                                        cbig_mni2009c_fsLR_R
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
#> 13  surface-transforms-v1 neuroatlas
#> 14  surface-transforms-v1 neuroatlas
#> 15  surface-transforms-v1 neuroatlas
#> 16  surface-transforms-v1 neuroatlas
#> 17  surface-projection-v1 neuroatlas
#> 18  surface-projection-v1 neuroatlas
#> 19  surface-projection-v1 neuroatlas
#> 20  surface-projection-v1 neuroatlas
#> 21  surface-projection-v1 neuroatlas
#> 22  surface-projection-v1 neuroatlas
#> 23  surface-projection-v1 neuroatlas
#> 24  surface-projection-v1 neuroatlas
#>                                                                                                                                                                                                                                    url
#> 1                                                                                                                                                                                                                                 <NA>
#> 2                                                                                                                                                                                                                                 <NA>
#> 3                                                                                     https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5
#> 4                                                                                     https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5
#> 5                                                                                                                                                                                                                                 <NA>
#> 6                                                                                                                                                                                                                                 <NA>
#> 7                                                                                                                                                                                                                                 <NA>
#> 8                                                                                                                                                                                                                                 <NA>
#> 9                                                                                                                                                                                                                                 <NA>
#> 10                                                                                                                                                                                                                                <NA>
#> 11                                                                                                                                                                                                                                <NA>
#> 12                                                                                                                                                                                                                                <NA>
#> 13                                                                                                                                                                                                                                <NA>
#> 14                                                                                                                                                                                                                                <NA>
#> 15                                                                                                                                                                                                                                <NA>
#> 16                                                                                                                                                                                                                                <NA>
#> 17 https://raw.githubusercontent.com/ThomasYeoLab/CBIG/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/lh.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat
#> 18 https://raw.githubusercontent.com/ThomasYeoLab/CBIG/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/lh.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat
#> 19 https://raw.githubusercontent.com/ThomasYeoLab/CBIG/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/rh.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat
#> 20 https://raw.githubusercontent.com/ThomasYeoLab/CBIG/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/rh.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat
#> 21 https://raw.githubusercontent.com/ThomasYeoLab/CBIG/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/lh.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat
#> 22 https://raw.githubusercontent.com/ThomasYeoLab/CBIG/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/lh.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat
#> 23 https://raw.githubusercontent.com/ThomasYeoLab/CBIG/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/rh.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat
#> 24 https://raw.githubusercontent.com/ThomasYeoLab/CBIG/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/rh.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat
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
#> 13                                                             <NA>         NA
#> 14                                                             <NA>         NA
#> 15                                                             <NA>         NA
#> 16                                                             <NA>         NA
#> 17 3961b1e1f04621f8c1961ac8e4e5385813e47579214e0ccd5e62d685265205fd    3584268
#> 18 3961b1e1f04621f8c1961ac8e4e5385813e47579214e0ccd5e62d685265205fd    3584268
#> 19 c44a8a824ade7c7c2203f2cd91cc6dced7c064d4371a45270cc50bb02d182319    3573633
#> 20 c44a8a824ade7c7c2203f2cd91cc6dced7c064d4371a45270cc50bb02d182319    3573633
#> 21 3961b1e1f04621f8c1961ac8e4e5385813e47579214e0ccd5e62d685265205fd    3584268
#> 22 3961b1e1f04621f8c1961ac8e4e5385813e47579214e0ccd5e62d685265205fd    3584268
#> 23 c44a8a824ade7c7c2203f2cd91cc6dced7c064d4371a45270cc50bb02d182319    3573633
#> 24 c44a8a824ade7c7c2203f2cd91cc6dced7c064d4371a45270cc50bb02d182319    3573633
#>                format                      convention qualification
#> 1                <NA>                            <NA>          <NA>
#> 2                <NA>                            <NA>          <NA>
#> 3             ants_h5         ants_image_pullback_ras        passed
#> 4             ants_h5         ants_image_pullback_ras        passed
#> 5                <NA>                            <NA>          <NA>
#> 6                <NA>                            <NA>          <NA>
#> 7                <NA>                            <NA>          <NA>
#> 8                <NA>                            <NA>          <NA>
#> 9                <NA>                            <NA>          <NA>
#> 10               <NA>                            <NA>          <NA>
#> 11               <NA>                            <NA>          <NA>
#> 12               <NA>                            <NA>          <NA>
#> 13 native_barycentric sphere_correspondence_fsaverage        passed
#> 14 native_barycentric sphere_correspondence_fsaverage        passed
#> 15 native_barycentric sphere_correspondence_fsaverage        passed
#> 16 native_barycentric sphere_correspondence_fsaverage        passed
#> 17          cbig_mat5      zero_based_voxel_to_ras_mm        passed
#> 18          cbig_mat5      zero_based_voxel_to_ras_mm        passed
#> 19          cbig_mat5      zero_based_voxel_to_ras_mm        passed
#> 20          cbig_mat5      zero_based_voxel_to_ras_mm        passed
#> 21          cbig_mat5      zero_based_voxel_to_ras_mm        passed
#> 22          cbig_mat5      zero_based_voxel_to_ras_mm        passed
#> 23          cbig_mat5      zero_based_voxel_to_ras_mm        passed
#> 24          cbig_mat5      zero_based_voxel_to_ras_mm        passed
#>                                                                          qualification_scope
#> 1                                                                                       <NA>
#> 2                                                                                       <NA>
#> 3                                                                      template_pair_1mm_2mm
#> 4                                                                      template_pair_1mm_2mm
#> 5                                                                                       <NA>
#> 6                                                                                       <NA>
#> 7                                                                                       <NA>
#> 8                                                                                       <NA>
#> 9                                                                                       <NA>
#> 10                                                                                      <NA>
#> 11                                                                                      <NA>
#> 12                                                                                      <NA>
#> 13                     native_closest_barycentric_exact_domains_continuous_label_probability
#> 14                     native_closest_barycentric_exact_domains_continuous_label_probability
#> 15                     native_closest_barycentric_exact_domains_continuous_label_probability
#> 16                     native_closest_barycentric_exact_domains_continuous_label_probability
#> 17 registration_fusion_exact_domains_1mm_2mm_continuous_label_probability_sampling_numerical
#> 18 registration_fusion_exact_domains_1mm_2mm_continuous_label_probability_sampling_numerical
#> 19 registration_fusion_exact_domains_1mm_2mm_continuous_label_probability_sampling_numerical
#> 20 registration_fusion_exact_domains_1mm_2mm_continuous_label_probability_sampling_numerical
#> 21 registration_fusion_exact_domains_1mm_2mm_continuous_label_probability_sampling_numerical
#> 22 registration_fusion_exact_domains_1mm_2mm_continuous_label_probability_sampling_numerical
#> 23 registration_fusion_exact_domains_1mm_2mm_continuous_label_probability_sampling_numerical
#> 24 registration_fusion_exact_domains_1mm_2mm_continuous_label_probability_sampling_numerical
#>                                                                                                                              qa_url
#> 1                                                                                                                              <NA>
#> 2                                                                                                                              <NA>
#> 3                                         https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/qa.json
#> 4                                         https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/qa.json
#> 5                                                                                                                              <NA>
#> 6                                                                                                                              <NA>
#> 7                                                                                                                              <NA>
#> 8                                                                                                                              <NA>
#> 9                                                                                                                              <NA>
#> 10                                                                                                                             <NA>
#> 11                                                                                                                             <NA>
#> 12                                                                                                                             <NA>
#> 13 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 14 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 15 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 16 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 17 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 18 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 19 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 20 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 21 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 22 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 23 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
#> 24 https://github.com/bbuchsbaum/neuroatlas/blob/v0.2.0/data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md
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
#> 13                                        upstream_download_only; see surface-assets-LICENSES.md
#> 14                                        upstream_download_only; see surface-assets-LICENSES.md
#> 15                                        upstream_download_only; see surface-assets-LICENSES.md
#> 16                                        upstream_download_only; see surface-assets-LICENSES.md
#> 17 https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md
#> 18 https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md
#> 19 https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md
#> 20 https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md
#> 21 https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md
#> 22 https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md
#> 23 https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md
#> 24 https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md
#>                                                      from_domain_id
#> 1                                                              <NA>
#> 2                                                              <NA>
#> 3                                                              <NA>
#> 4                                                              <NA>
#> 5                                                              <NA>
#> 6                                                              <NA>
#> 7                                                              <NA>
#> 8                                                              <NA>
#> 9                                                              <NA>
#> 10                                                             <NA>
#> 11                                                             <NA>
#> 12                                                             <NA>
#> 13 db673d691ff6afb99706cd39db2511316deb6b345e43d7fa8cc9734a5142e110
#> 14 662cdee2b9272e0943a8f6a994407782a1b70bde91c943efb61ffc933e1ac912
#> 15 fb6c4cbad68dc5d6881490379e6c2dcff26aac1f1841688121484c6f98cd2a0b
#> 16 1816e589ab705013848a72961864f8618019d7a4ddbf4cb0aad59e09eacd37ac
#> 17                                                             <NA>
#> 18                                                             <NA>
#> 19                                                             <NA>
#> 20                                                             <NA>
#> 21                                                             <NA>
#> 22                                                             <NA>
#> 23                                                             <NA>
#> 24                                                             <NA>
#>                                                        to_domain_id
#> 1                                                              <NA>
#> 2                                                              <NA>
#> 3                                                              <NA>
#> 4                                                              <NA>
#> 5                                                              <NA>
#> 6                                                              <NA>
#> 7                                                              <NA>
#> 8                                                              <NA>
#> 9                                                              <NA>
#> 10                                                             <NA>
#> 11                                                             <NA>
#> 12                                                             <NA>
#> 13 662cdee2b9272e0943a8f6a994407782a1b70bde91c943efb61ffc933e1ac912
#> 14 db673d691ff6afb99706cd39db2511316deb6b345e43d7fa8cc9734a5142e110
#> 15 1816e589ab705013848a72961864f8618019d7a4ddbf4cb0aad59e09eacd37ac
#> 16 fb6c4cbad68dc5d6881490379e6c2dcff26aac1f1841688121484c6f98cd2a0b
#> 17 db673d691ff6afb99706cd39db2511316deb6b345e43d7fa8cc9734a5142e110
#> 18 662cdee2b9272e0943a8f6a994407782a1b70bde91c943efb61ffc933e1ac912
#> 19 fb6c4cbad68dc5d6881490379e6c2dcff26aac1f1841688121484c6f98cd2a0b
#> 20 1816e589ab705013848a72961864f8618019d7a4ddbf4cb0aad59e09eacd37ac
#> 21 db673d691ff6afb99706cd39db2511316deb6b345e43d7fa8cc9734a5142e110
#> 22 662cdee2b9272e0943a8f6a994407782a1b70bde91c943efb61ffc933e1ac912
#> 23 fb6c4cbad68dc5d6881490379e6c2dcff26aac1f1841688121484c6f98cd2a0b
#> 24 1816e589ab705013848a72961864f8618019d7a4ddbf4cb0aad59e09eacd37ac
#>                         method                          engine_revision
#> 1                         <NA>                                     <NA>
#> 2                         <NA>                                     <NA>
#> 3                         <NA>                                     <NA>
#> 4                         <NA>                                     <NA>
#> 5                         <NA>                                     <NA>
#> 6                         <NA>                                     <NA>
#> 7                         <NA>                                     <NA>
#> 8                         <NA>                                     <NA>
#> 9                         <NA>                                     <NA>
#> 10                        <NA>                                     <NA>
#> 11                        <NA>                                     <NA>
#> 12                        <NA>                                     <NA>
#> 13  native_closest_barycentric 933edddda462593941e167726e8aaa7168ff103a
#> 14  native_closest_barycentric 933edddda462593941e167726e8aaa7168ff103a
#> 15  native_closest_barycentric 933edddda462593941e167726e8aaa7168ff103a
#> 16  native_closest_barycentric 933edddda462593941e167726e8aaa7168ff103a
#> 17 cbig_rf_ants_linear_nearest 933edddda462593941e167726e8aaa7168ff103a
#> 18 cbig_rf_ants_linear_nearest 933edddda462593941e167726e8aaa7168ff103a
#> 19 cbig_rf_ants_linear_nearest 933edddda462593941e167726e8aaa7168ff103a
#> 20 cbig_rf_ants_linear_nearest 933edddda462593941e167726e8aaa7168ff103a
#> 21 cbig_rf_ants_linear_nearest 933edddda462593941e167726e8aaa7168ff103a
#> 22 cbig_rf_ants_linear_nearest 933edddda462593941e167726e8aaa7168ff103a
#> 23 cbig_rf_ants_linear_nearest 933edddda462593941e167726e8aaa7168ff103a
#> 24 cbig_rf_ants_linear_nearest 933edddda462593941e167726e8aaa7168ff103a
#>                                                   input_lock_sha256 hemisphere
#> 1                                                              <NA>       <NA>
#> 2                                                              <NA>       <NA>
#> 3                                                              <NA>       <NA>
#> 4                                                              <NA>       <NA>
#> 5                                                              <NA>       <NA>
#> 6                                                              <NA>       <NA>
#> 7                                                              <NA>       <NA>
#> 8                                                              <NA>       <NA>
#> 9                                                              <NA>       <NA>
#> 10                                                             <NA>       <NA>
#> 11                                                             <NA>       <NA>
#> 12                                                             <NA>       <NA>
#> 13 efcd4d03f1361ff3be350f5fa2f640b146677de5cd780ae2a5b523c6151c97e0          L
#> 14 efcd4d03f1361ff3be350f5fa2f640b146677de5cd780ae2a5b523c6151c97e0          L
#> 15 efcd4d03f1361ff3be350f5fa2f640b146677de5cd780ae2a5b523c6151c97e0          R
#> 16 efcd4d03f1361ff3be350f5fa2f640b146677de5cd780ae2a5b523c6151c97e0          R
#> 17                                                             <NA>          L
#> 18                                                             <NA>          L
#> 19                                                             <NA>          R
#> 20                                                             <NA>          R
#> 21                                                             <NA>          L
#> 22                                                             <NA>          L
#> 23                                                             <NA>          R
#> 24                                                             <NA>          R
#>    source_representation target_representation executable
#> 1                 volume                volume       TRUE
#> 2                 volume                volume       TRUE
#> 3                 volume                volume       TRUE
#> 4                 volume                volume       TRUE
#> 5                surface               surface      FALSE
#> 6                surface               surface      FALSE
#> 7                surface               surface      FALSE
#> 8                surface               surface      FALSE
#> 9                surface               surface      FALSE
#> 10               surface               surface      FALSE
#> 11                volume               surface      FALSE
#> 12               surface                volume      FALSE
#> 13               surface               surface       TRUE
#> 14               surface               surface       TRUE
#> 15               surface               surface       TRUE
#> 16               surface               surface       TRUE
#> 17                volume               surface       TRUE
#> 18                volume               surface       TRUE
#> 19                volume               surface       TRUE
#> 20                volume               surface       TRUE
#> 21                volume               surface       TRUE
#> 22                volume               surface       TRUE
#> 23                volume               surface       TRUE
#> 24                volume               surface       TRUE
```
