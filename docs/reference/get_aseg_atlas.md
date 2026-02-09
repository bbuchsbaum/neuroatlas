# Get the FreeSurfer Subcortical Atlas (ASEG)

Loads and returns the FreeSurfer subcortical segmentation (ASEG) atlas,
which provides probabilistic labels for key subcortical structures in
the brain. The atlas includes bilateral structures such as the thalamus,
caudate, putamen, and limbic regions, as well as midline structures like
the brainstem.

## Usage

``` r
get_aseg_atlas(outspace = NULL)
```

## Arguments

- outspace:

  Optional `NeuroSpace` object specifying the desired output space for
  resampling the atlas. If NULL (default), returns the atlas in its
  native space.

## Value

A list with classes 'aseg' and 'atlas' containing:

- atlas:

  A `NeuroVol` object containing the 3D volume of atlas labels

- cmap:

  A data frame with RGB color specifications for each region

- ids:

  Integer vector of region IDs present in the atlas

- labels:

  Character vector of anatomical labels corresponding to each ID

- hemi:

  Character vector indicating hemisphere ('left', 'right', or NA) for
  each region

## Details

The ASEG atlas is derived from FreeSurfer's automatic subcortical
segmentation algorithm and has been transformed into standard space.
Each voxel contains an integer ID corresponding to a specific anatomical
structure. The atlas includes major subcortical structures for both
hemispheres:

- Bilateral deep gray structures (thalamus, caudate, putamen, pallidum)

- Limbic structures (hippocampus, amygdala)

- Ventral structures (nucleus accumbens, ventral diencephalon)

- Midline structures (brainstem)

## References

Fischl, B., et al. (2002). Whole brain segmentation: automated labeling
of neuroanatomical structures in the human brain. Neuron, 33(3),
341-355.

## See also

[`map_atlas`](map_atlas.md) for mapping values onto atlas regions
[`get_roi`](get_roi.md) for extracting specific regions of interest

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the atlas in native space
aseg <- get_aseg_atlas()

# View the available region labels
aseg$labels

# Get the unique region IDs
aseg$ids
} # }
```
