# Load Olsen MTL Atlas

Loads the Olsen medial temporal lobe atlas and optionally resamples it
to a different space.

## Usage

``` r
get_olsen_mtl(outspace = NULL)
```

## Arguments

- outspace:

  Optional `NeuroSpace` object specifying desired output space. If NULL
  (default), returns atlas in native 1mm MNI space.

## Value

A list with class 'atlas' containing the MTL parcellation

## See also

[`get_hipp_atlas`](get_hipp_atlas.md) for hippocampus-specific
parcellation

## Examples

``` r
# \donttest{
# Load in native space
mtl <- get_olsen_mtl()

# Load and resample to MNI152NLin2009cAsym space (requires neuroim2)
# space <- neuroim2::read_template_space("MNI152NLin2009cAsym")
# mtl_resampled <- get_olsen_mtl(outspace = space)
# }
```
