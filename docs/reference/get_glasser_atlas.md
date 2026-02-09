# Load Glasser Atlas

Retrieves and loads the Glasser360 cortical parcellation atlas from the
PennBBL repository. The atlas provides a detailed parcellation of the
human cerebral cortex based on multi-modal neuroimaging data.

## Usage

``` r
get_glasser_atlas(outspace = NULL)
```

## Source

Atlas files are downloaded from:
<https://github.com/PennBBL/xcpEngine/tree/master/atlas/glasser360>

## Arguments

- outspace:

  Optional `NeuroSpace` object specifying desired output space. If
  provided, the atlas will be resampled to this space. Default: NULL

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
The atlas is downloaded from the PennBBL xcpEngine repository and
includes:

- Volume data in MNI space

- Region labels and hemisphere information

- Color specifications for visualization

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
