# Internal Helper to Convert File Path to NeuroVol

Reads a NIfTI file into a
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
object.

## Usage

``` r
.as_neurovol_unmemoised(file_path)
```

## Arguments

- file_path:

  A character string file path to a NIfTI file.

## Value

A
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
object.
