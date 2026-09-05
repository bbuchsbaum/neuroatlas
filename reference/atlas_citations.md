# Bibliographic References for an Atlas or Template

Convert the stored citation table to R bibliography entries. These
accessors never resolve DOIs or contact a service. References without
complete article metadata remain \`Misc\` entries with their recorded
title and URL.

## Usage

``` r
atlas_citations(x, role = NULL)

template_citations(x, role = NULL)
```

## Arguments

- x:

  An atlas or loaded template object.

- role:

  Optional citation role(s), such as \`"atlas"\`, \`"distribution"\`,
  \`"template"\`, or \`"software"\`. \`NULL\` includes all roles.

## Value

A \`bibentry\` vector, suitable for \[utils::toBibtex()\]. Each entry's
note identifies its role, and its key is derived from the stored DOI,
URL, or title. The table with separate role rows is available through
\[atlas_metadata()\] or \[template_metadata()\].

## Examples

``` r
refs <- atlas_citations(get_aseg_atlas())
refs
#> Fischl B, others (2002). “Whole brain segmentation: automated labeling
#> of neuroanatomical structures in the human brain.” _Neuron_.
#> doi:10.1016/S0896-6273(02)00569-X
#> <https://doi.org/10.1016/S0896-6273%2802%2900569-X>. Role: atlas,
#> <https://doi.org/10.1016/S0896-6273(02)00569-X>.
utils::toBibtex(refs)
#> @Article{neuroatlas_10_1016_s0896_6273_02_00569_x,
#>   title = {Whole brain segmentation: automated labeling of neuroanatomical structures in the human brain},
#>   author = {Bruce Fischl and others},
#>   year = {2002},
#>   journal = {Neuron},
#>   doi = {10.1016/S0896-6273(02)00569-X},
#>   url = {https://doi.org/10.1016/S0896-6273(02)00569-X},
#>   note = {Role: atlas},
#> }
```
