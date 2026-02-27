# Schaefer Atlas with 17 Networks and 200 Parcels

Pre-loaded Schaefer cortical parcellation with 200 regions organized
into 17 functional networks. This atlas divides the cerebral cortex
based on resting-state functional connectivity patterns.

## Usage

``` r
Schaefer17_200
```

## Format

A list with class 'schaefer' and 'atlas' containing:

- name:

  Character string "schaefer17_200"

- atlas:

  A `ClusteredNeuroVol` object with the parcellation

- cmap:

  Data frame with RGB color values for visualization

- ids:

  Integer vector of region IDs (1:200)

- labels:

  Character vector of region labels

- orig_labels:

  Original Schaefer labels

- network:

  Network assignment for each region

- hemi:

  Hemisphere designation ('left' or 'right')

## Source

<https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal>

## References

Schaefer, A., et al. (2018). Local-Global Parcellation of the Human
Cerebral Cortex from Intrinsic Functional Connectivity MRI. Cerebral
Cortex, 28(9), 3095-3114.

## Examples

``` r
# \donttest{
data(Schaefer17_200)
print(Schaefer17_200)
#> # ggseg atlas
#>    hemi  area                                      side    label atlas     ggseg
#>    <chr> <chr>                                     <chr>   <chr> <chr> <list<ti>
#>  1 left  Background.FreeSurfer_Defined_Medial_Wall lateral Back… Scha… [136 × 4]
#>  2 left  VisCent_ExStr_3                           lateral 17Ne… Scha… [130 × 4]
#>  3 left  VisCent_ExStr_5                           lateral 17Ne… Scha… [142 × 4]
#>  4 left  VisCent_ExStr_6                           lateral 17Ne… Scha… [148 × 4]
#>  5 left  VisCent_ExStr_7                           lateral 17Ne… Scha… [149 × 4]
#>  6 left  VisCent_ExStr_8                           lateral 17Ne… Scha… [140 × 4]
#>  7 left  VisCent_ExStr_9                           lateral 17Ne… Scha… [149 × 4]
#>  8 left  VisCent_ExStr_11                          lateral 17Ne… Scha… [146 × 4]
#>  9 left  SomMotA_2                                 lateral 17Ne… Scha… [145 × 4]
#> 10 left  SomMotA_3                                 lateral 17Ne… Scha… [161 × 4]
#> # ℹ 390 more rows
table(Schaefer17_200$network)
#> Warning: Unknown or uninitialised column: `network`.
#> < table of extent 0 >
# }
```
