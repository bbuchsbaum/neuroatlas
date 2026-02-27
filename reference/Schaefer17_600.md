# Schaefer Atlas with 17 Networks and 600 Parcels

Pre-loaded Schaefer cortical parcellation with 600 regions organized
into 17 functional networks. This atlas divides the cerebral cortex
based on resting-state functional connectivity patterns.

## Usage

``` r
Schaefer17_600
```

## Format

A list with class 'schaefer' and 'atlas' containing:

- name:

  Character string "schaefer17_600"

- atlas:

  A `ClusteredNeuroVol` object with the parcellation

- cmap:

  Data frame with RGB color values for visualization

- ids:

  Integer vector of region IDs (1:600)

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
data(Schaefer17_600)
print(Schaefer17_600)
#> # ggseg atlas
#>    hemi  area                                      side    label atlas     ggseg
#>    <chr> <chr>                                     <chr>   <chr> <chr> <list<ti>
#>  1 left  Background.FreeSurfer_Defined_Medial_Wall lateral Back… Scha… [133 × 4]
#>  2 left  VisCent_ExStr_5                           lateral 17Ne… Scha… [136 × 4]
#>  3 left  VisCent_ExStr_7                           lateral 17Ne… Scha… [140 × 4]
#>  4 left  VisCent_ExStr_8                           lateral 17Ne… Scha… [141 × 4]
#>  5 left  VisCent_ExStr_9                           lateral 17Ne… Scha… [144 × 4]
#>  6 left  VisCent_ExStr_10                          lateral 17Ne… Scha… [141 × 4]
#>  7 left  VisCent_ExStr_11                          lateral 17Ne… Scha… [131 × 4]
#>  8 left  VisCent_ExStr_12                          lateral 17Ne… Scha… [136 × 4]
#>  9 left  VisCent_ExStr_14                          lateral 17Ne… Scha… [141 × 4]
#> 10 left  VisCent_ExStr_16                          lateral 17Ne… Scha… [140 × 4]
#> # ℹ 570 more rows
table(Schaefer17_600$network)
#> Warning: Unknown or uninitialised column: `network`.
#> < table of extent 0 >
# }
```
