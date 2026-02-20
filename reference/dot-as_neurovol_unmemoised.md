# Internal Helper to Convert Python Object/Path to NeuroVol

This function takes a file path (string) or a Python object that can be
resolved to a NIfTI file path (e.g., a Python Path object from
TemplateFlow) and reads it into a
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
object.

## Usage

``` r
.as_neurovol_unmemoised(path_or_py_obj)
```

## Arguments

- path_or_py_obj:

  A file path string or a Python object (e.g., `pathlib.Path`).

## Value

A
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
object.
