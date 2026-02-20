# Olsen Medial Temporal Lobe Atlas

A detailed parcellation atlas of the medial temporal lobe (MTL) regions,
including hippocampus and surrounding cortical areas, based on the work
of Rosanna Olsen and colleagues.

## Usage

``` r
olsen_mtl
```

## Format

A list with class 'atlas' containing:

- name:

  Character string identifying the atlas

- atlas:

  NeuroVol object containing the parcellation in 1mm MNI space

- labels:

  Character vector of anatomical region labels

- orig_labels:

  Full region labels including hemisphere information

- ids:

  Integer vector of region IDs (1:16)

- hemi:

  Character vector indicating hemisphere ('left' or 'right')

## Source

Olsen, R. K., et al. (2013). The role of relational binding in item
memory: Evidence from face recognition in a case of developmental
amnesia. Journal of Neuroscience, 33(36), 14107-14111.

## Details

The atlas provides a detailed segmentation of MTL structures in MNI
space at 1mm resolution. It includes bilateral parcellation of:

- Hippocampal subfields

- Perirhinal cortex

- Entorhinal cortex

- Parahippocampal cortex

## Examples

``` r
# \donttest{
# Load the atlas data
data(olsen_mtl)

# View available regions
olsen_mtl$labels
#>  [1] "Ant_Hipp"  "CA1"       "CA3_DG"    "ERC"       "PHC"       "Post_Hipp"
#>  [7] "PRC"       "Sub"       "Ant_Hipp"  "CA1"       "CA3_DG"    "ERC"      
#> [13] "PHC"       "Post_Hipp" "PRC"       "Sub"      

# Check distribution across hemispheres
table(olsen_mtl$hemi)
#> 
#>  left right 
#>     8     8 
# }
```
