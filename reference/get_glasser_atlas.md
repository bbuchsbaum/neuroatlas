# Load Glasser Atlas

Retrieves and loads a volumetric representation of the Glasser/HCP-MMP
1.0 cortical parcellation atlas.

## Usage

``` r
get_glasser_atlas(outspace = NULL, source = c("mni2009c", "xcpengine"))
```

## Source

Source-specific links are recorded in `atlas_ref(atlas)$provenance`.

## Arguments

- outspace:

  Optional `NeuroSpace` object specifying desired output space. If
  provided, the atlas will be resampled to this space. Default: NULL

- source:

  Volume source to use. One of `"mni2009c"` (default) or `"xcpengine"`.

## Value

A list with class 'glasser' and 'atlas' containing:

- name:

  Character string "Glasser360"

- atlas:

  A `ClusteredNeuroVol` object containing the parcellation

- cmap:

  Data frame with RGB color specifications for each region

- ids:

  Integer vector of region IDs (1:360)

- labels:

  Character vector of anatomical labels

- hemi:

  Character vector indicating hemisphere ('left' or 'right')

## Details

The Glasser atlas divides each hemisphere into 180 areas (360 total)
based on cortical architecture, function, connectivity, and topography.

Supported sources:

- `"mni2009c"` (default): MNI152NLin2009cAsym-provenance volume file
  (`MMP_in_MNI_corr.nii.gz`).

- `"xcpengine"`: legacy xcpEngine Glasser360 volume
  (`glasser360MNI.nii.gz`) with less explicit template provenance.

If `source = "mni2009c"` is unavailable at runtime, the loader
automatically falls back to `"xcpengine"` and marks confidence as
`"uncertain"`.

Region labels are read from the xcpEngine node-name table to provide
stable parcel naming across sources.

## References

Glasser, M. F., et al. (2016). A multi-modal parcellation of human
cerebral cortex. Nature, 536(7615), 171-178.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load atlas in native space
atlas <- get_glasser_atlas()

# View region labels
head(atlas$labels)

# Check number of regions per hemisphere
table(atlas$hemi)
} # }
```
