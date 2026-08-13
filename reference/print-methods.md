# Print Methods for neuroatlas Objects

Print methods for various atlas objects in the neuroatlas package

## Usage

``` r
# S3 method for class 'atlas'
print(x, ...)

# S3 method for class 'atlas_provenance'
print(x, ...)

# S3 method for class 'glasser'
print(x, ...)

# S3 method for class 'schaefer'
print(x, ...)
```

## Arguments

- x:

  An atlas object (atlas, glasser, schaefer, etc.)

- ...:

  Additional arguments passed to print

## Value

Invisibly returns `x`.

The object is returned invisibly

## Examples

``` r
atlas <- get_aseg_atlas()
print(atlas)
#> ── Atlas Summary ─────────────────────────────────────────── 
#> 
#> ❯ Name:   ASEG
#> ❯ Model:  FreeSurferASEG [volume]
#> ❯ Space:  MNI152NLin6Asym
#> ❯ Source: bundled_extdata
#> ❯ Provenance: 1 artifacts, 1 history steps
#> ❯ Dimensions: 193 x 229 x 193
#> ❯ Regions: 17
#> 
#> Structure Distribution:
#> |- Left hemisphere:     7
#> |- Right hemisphere:    8
#> \- Bilateral/Midline:   2
#> 
#> ────────────────────────────────────────────────────────────
```
