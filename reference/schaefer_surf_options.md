# List supported Schaefer surface atlas variants

Return a data frame enumerating the currently supported Schaefer surface
atlas variants, including their CBIG and TemplateFlow identifiers.

## Usage

``` r
schaefer_surf_options()
```

## Value

A data frame with columns:

- `space`: Schaefer surface space (fsaverage, fsaverage5, fsaverage6).

- `parcels`: Number of parcels.

- `networks`: Number of networks.

- `surf`: Surface type ("inflated", "white", "pial").

- `cbig_space`: CBIG FreeSurfer5.3 subfolder.

- `template_id`, `tf_resolution`, `tf_density`: TemplateFlow identifiers
  used by [`get_surface_template()`](get_template.md).

## Examples

``` r
opts <- schaefer_surf_options()
head(opts)
#>        space parcels networks     surf cbig_space template_id tf_resolution
#> 1  fsaverage     100        7 inflated  fsaverage   fsaverage          <NA>
#> 2 fsaverage5     100        7 inflated fsaverage5   fsaverage            05
#> 3 fsaverage6     100        7 inflated fsaverage6   fsaverage            06
#> 4  fsaverage     200        7 inflated  fsaverage   fsaverage          <NA>
#> 5 fsaverage5     200        7 inflated fsaverage5   fsaverage            05
#> 6 fsaverage6     200        7 inflated fsaverage6   fsaverage            06
#>   tf_density
#> 1       164k
#> 2        10k
#> 3        41k
#> 4       164k
#> 5        10k
#> 6        41k
```
