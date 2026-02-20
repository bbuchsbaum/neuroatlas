# Merge Two Brain Atlases

Combines two brain atlases into a single unified atlas object,
preserving all region information and adjusting region IDs to prevent
conflicts. This is useful for creating composite atlases that combine
different parcellation schemes.

## Usage

``` r
merge_atlases(atlas1, atlas2)
```

## Arguments

- atlas1:

  The first atlas object to merge

- atlas2:

  The second atlas object to merge

## Value

A new atlas object containing:

- name:

  Combined names of input atlases (atlas1::atlas2)

- atlas:

  Combined `ClusteredNeuroVol` object

- cmap:

  Combined colormap for all regions

- ids:

  Adjusted vector of all region IDs

- labels:

  Combined vector of region labels

- orig_labels:

  Original labels from both atlases

- hemi:

  Combined hemisphere designations

## Details

The merging process:

- Verifies that both atlases have the same dimensions

- Adjusts region IDs in the second atlas to avoid overlap

- Combines color maps, labels, and hemisphere information

- Creates a new ClusteredNeuroVol object for the merged atlas

## See also

[`get_aseg_atlas`](get_aseg_atlas.md), [`get_roi`](get_roi.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load two atlases
atlas1 <- get_aseg_atlas()
atlas2 <- get_aseg_atlas()

# Merge the atlases
merged <- merge_atlases(atlas1, atlas2)

# Check the combined regions
print(merged)
} # }
```
