# Print Method for Transform Plans

Print Method for Transform Plans

## Usage

``` r
# S3 method for class 'atlas_transform_plan'
print(x, ...)
```

## Arguments

- x:

  An \`atlas_transform_plan\` object.

- ...:

  Unused.

## Value

Invisibly returns \`x\`.

## Examples

``` r
p <- atlas_transform_plan("MNI305", "MNI152")
print(p)
#> <atlas_transform_plan>
#>   from_space: MNI305 
#>   to_space: MNI152 
#>   n_steps: 1 
#>   status: available 
#>   confidence: exact 
```
