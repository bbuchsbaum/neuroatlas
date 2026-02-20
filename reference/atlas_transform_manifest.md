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
#>             from_space            to_space  transform_type           backend
#> 1               MNI305              MNI152          affine   internal_affine
#> 2               MNI152              MNI305          affine   internal_affine
#> 3      MNI152NLin6Asym MNI152NLin2009cAsym  nonlinear_warp templateflow_ants
#> 4  MNI152NLin2009cAsym     MNI152NLin6Asym  nonlinear_warp templateflow_ants
#> 5            fsaverage          fsaverage6 sphere_resample         sphere_nn
#> 6           fsaverage6           fsaverage sphere_resample         sphere_nn
#> 7            fsaverage          fsaverage5 sphere_resample         sphere_nn
#> 8           fsaverage5           fsaverage sphere_resample         sphere_nn
#> 9            fsaverage            fsLR_32k sphere_resample         workbench
#> 10            fsLR_32k           fsaverage sphere_resample         workbench
#> 11 MNI152NLin2009cAsym           fsaverage        vol2surf         neurosurf
#> 12           fsaverage MNI152NLin2009cAsym        surf2vol       ribbon_fill
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
#>                                                       data_files    status
#> 1                                                           <NA> available
#> 2                                                           <NA> available
#> 3  from-MNI152NLin6Asym_to-MNI152NLin2009cAsym_mode-image_xfm.h5   planned
#> 4  from-MNI152NLin2009cAsym_to-MNI152NLin6Asym_mode-image_xfm.h5   planned
#> 5                                   fsaverage/surf/?h.sphere.reg available
#> 6                                   fsaverage/surf/?h.sphere.reg available
#> 7                                   fsaverage/surf/?h.sphere.reg available
#> 8                                   fsaverage/surf/?h.sphere.reg available
#> 9             fs_LR-deformed_to-fsaverage.?H.sphere.reg.surf.gii   planned
#> 10                     fsaverage_to-fs_LR.?H.sphere.reg.surf.gii   planned
#> 11                                                          <NA>   planned
#> 12                                                          <NA>   planned
#>                                            notes
#> 1                 FreeSurfer mni152.register.dat
#> 2      Inverse of FreeSurfer mni152.register.dat
#> 3                    TemplateFlow composite warp
#> 4            TemplateFlow composite warp inverse
#> 5  Downsample 164k to 41k on registration sphere
#> 6    Upsample 41k to 164k on registration sphere
#> 7  Downsample 164k to 10k on registration sphere
#> 8    Upsample 10k to 164k on registration sphere
#> 9          HCP sphere registration via Workbench
#> 10         HCP sphere registration via Workbench
#> 11              Requires white and pial surfaces
#> 12             Requires ribbon mask construction
```
