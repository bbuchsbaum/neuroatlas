# Subcortical Atlas Options (TemplateFlow-backed)

Returns a tibble describing the supported subcortical atlases built and
harmonized by the AtlasPack resource (HCP thalamus, MDTB10 cerebellum,
CIT168 subcortex, and HCP hippocampus/amygdala). Each row includes the
TemplateFlow atlas identifier, supported template spaces, and default
resolution/desc parameters used to fetch the data.

## Usage

``` r
subcortical_atlas_options()
```

## Value

A tibble with columns:

- `id`: canonical neuroatlas identifier

- `atlas`: TemplateFlow `atlas` field

- `label`: human-readable name

- `default_space`: default TemplateFlow space

- `spaces`: list-column of allowed spaces

- `default_resolution`: default TemplateFlow resolution string

- `resolutions`: list-column of allowed resolutions

- `default_desc`: optional TemplateFlow `desc` value

## References

1\. AtlasPack resource for harmonized atlases:
<https://github.com/PennLINC/AtlasPack> 2. Najdenovska E. et al. (2018)
Scientific Data, HCP thalamus. 3. King M. et al. (2019) Nature
Neuroscience, MDTB10 cerebellum. 4. Pauli W.M. et al. (2018) Scientific
Data, CIT168 subcortex. 5. Glasser M.F. et al. (2013) Neuroimage, HCP
subcortical structures.

## Examples

``` r
opts <- subcortical_atlas_options()
opts
#> # A tibble: 4 × 8
#>   id           atlas   label default_space spaces default_resolution resolutions
#>   <chr>        <chr>   <chr> <chr>         <list> <chr>              <list>     
#> 1 cit168       CIT168  CIT1… MNI152NLin6A… <chr>  01                 <chr [1]>  
#> 2 hcp_thalamus hcptha… HCP … MNI152NLin6A… <chr>  01                 <chr [1]>  
#> 3 mdtb10       MDTB10  MDTB… MNI152NLin6A… <chr>  01                 <chr [1]>  
#> 4 hcp_hippamyg HPandA… HCP … MNI152NLin6A… <chr>  01                 <chr [2]>  
#> # ℹ 1 more variable: default_desc <chr>
```
