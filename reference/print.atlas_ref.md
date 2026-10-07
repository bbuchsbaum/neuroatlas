# Print Method for Atlas References

Print Method for Atlas References

## Usage

``` r
# S3 method for class 'atlas_ref'
print(x, ...)
```

## Arguments

- x:

  An \`atlas_ref\` object.

- ...:

  Unused.

## Value

Invisibly returns \`x\`.

## Examples

``` r
ref <- new_atlas_ref("toy", "two-regions",
  template_space = "MNI152NLin6Asym", coord_space = "MNI152"
)
print(ref)
#> <atlas_ref>
#>   family: toy 
#>   model: two-regions 
#>   representation: volume 
#>   template_space: MNI152NLin6Asym 
#>   coord_space: MNI152 
#>   confidence: uncertain 
```
