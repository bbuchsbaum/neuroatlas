# Print Method for Atlas Alignment Results

Print Method for Atlas Alignment Results

## Usage

``` r
# S3 method for class 'atlas_alignment'
print(x, ...)
```

## Arguments

- x:

  An \`atlas_alignment\` object.

- ...:

  Unused.

## Value

Invisibly returns \`x\`.

## Examples

``` r
ref <- new_atlas_ref(
  family = "schaefer",
  model = "Schaefer2018",
  representation = "volume",
  template_space = "MNI152NLin6Asym",
  coord_space = "MNI152",
  confidence = "high"
)
a <- structure(list(atlas_ref = ref), class = c("schaefer", "atlas"))
print(atlas_alignment(a, a))
#> <atlas_alignment>
#>   compatible: TRUE 
#>   relation: identical 
#>   method: identity 
#>   confidence: exact 
#>   status: available 
#>   requires_transform: FALSE 
```
