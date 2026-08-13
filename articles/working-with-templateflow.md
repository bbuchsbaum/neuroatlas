# Working with TemplateFlow in neuroatlas

[TemplateFlow](https://www.templateflow.org/) gives neuroimaging
workflows a shared, queryable archive of templates and related assets.
The question is usually specific: *which file describes this tissue,
surface, or atlas in this template space and resolution?* `neuroatlas`
translates that query into either a file path or a
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html).

The current backend is the pure-R `templateflow` package. Python,
reticulate, and a connection object are not part of the supported setup.

## What do you need before you start?

Install `neuroatlas` with its `templateflow` import. You can then check
the backend and inspect its disk-cache location without downloading an
image:

``` r

library(neuroatlas)

requireNamespace("templateflow", quietly = TRUE)
show_templateflow_cache_path()
#> [1] "/home/runner/.cache/R/neuroatlas/templateflow"
```

[`create_templateflow()`](../reference/create_templateflow.md) and
[`install_templateflow()`](../reference/install_templateflow.md) are
deprecated migration stubs. New code should call
[`get_template()`](../reference/get_template.md),
[`tflow_spaces()`](../reference/tflow_spaces.md), and
[`tflow_files()`](../reference/tflow_files.md) directly.

## How do you fetch one template image?

This is the ordinary path:

``` r

brain <- get_template(
  space = "MNI152NLin2009cAsym",
  variant = "brain",
  modality = "T1w",
  resolution = 2
)
brain
```

The result is a `NeuroVol`: voxel values plus dimensions, spacing,
origin, and affine geometry. If another tool should read the file,
request the path instead:

``` r

brain_path <- get_template(
  space = "MNI152NLin2009cAsym",
  variant = "brain",
  modality = "T1w",
  resolution = 2,
  path_only = TRUE
)
brain_path
```

These chunks are not run during package builds because the first call
may use the network and write to the TemplateFlow cache. They are
intentionally shown without fabricated output.

## How do the query fields fit together?

TemplateFlow filenames use BIDS-like entities.
[`get_template()`](../reference/get_template.md) exposes two levels of
control:

| Argument | Meaning | Example |
|----|----|----|
| `space` | template coordinate system | `"MNI152NLin2009cAsym"` |
| `resolution` | volumetric resolution in millimetres | `2` |
| `variant` | convenient high-level asset type | `"brain"`, `"mask"`, `"probseg"` |
| `modality` | image suffix when applicable | `"T1w"`, `"T2w"` |
| `label` | tissue or structure label | `"GM"`, `"WM"`, `"CSF"` |
| `atlas` | atlas entity | `"Schaefer2018"` |
| `desc`, `suffix` | explicit low-level entities | `"brain"`, `"dseg"` |

Use the high-level `variant` interface for common images:

``` r

brain_mask <- get_template(
  "MNI152NLin2009cAsym",
  variant = "mask",
  resolution = 2
)

gm_probability <- get_template(
  "MNI152NLin2009cAsym",
  variant = "probseg",
  label = "GM",
  resolution = 2
)
```

Use explicit entities when the archive contains a specialised asset:

``` r

schaefer_path <- get_template(
  space = "MNI152NLin2009cAsym",
  atlas = "Schaefer2018",
  desc = "100Parcels7Networks",
  suffix = "dseg",
  resolution = 2,
  path_only = TRUE
)
```

If one argument has several values,
[`get_template()`](../reference/get_template.md) returns a named list:

``` r

templates <- get_template(
  space = "MNI152NLin2009cAsym",
  variant = "brain",
  resolution = c(1, 2),
  path_only = TRUE
)
names(templates)
```

Only one parameter may be vectorised in a call. For a grid of queries,
iterate explicitly so every requested combination is visible in your
code.

## How do you discover what exists?

[`tflow_spaces()`](../reference/tflow_spaces.md) lists registered
template IDs; its optional regular expression filters the result:

``` r

tflow_spaces(pattern = "^MNI")
```

[`tflow_files()`](../reference/tflow_files.md) is the lower-level
inventory query. It returns paths matching the supplied entities and is
useful before you choose one exact file:

``` r

tflow_files(
  "MNI152NLin2009cAsym",
  query_args = list(
    suffix = "T1w",
    resolution = 2
  )
)
```

Archive contents evolve independently of this package. Discover fields
from the live inventory rather than treating a vignette’s example roster
as a permanent catalogue.

## How do you fetch surface geometry?

Surface queries use hemisphere, density or resolution, and a surface
suffix. [`get_surface_template()`](../reference/get_template.md) returns
a file path by default:

``` r

left_midthickness <- get_surface_template(
  template_id = "fsLR",
  surface_type = "midthickness",
  hemi = "L",
  density = "32k"
)
left_midthickness
```

[`load_surface_template()`](../reference/load_surface_template.md) reads
the geometry for use with `neurosurf`:

``` r

geometry <- load_surface_template(
  template_id = "fsLR",
  surface_type = "midthickness",
  hemi = "L",
  density = "32k"
)
geometry
```

Use [`vignette("surface-templates")`](../articles/surface-templates.md)
for the distinction between geometry, per-vertex data, and labelled
surface atlases.

## Does a matching grid mean a matching space?

No. A TemplateFlow identifier describes an anatomical template, not
merely an array shape. Two images can have equal dimensions and still
require a spatial transform; two grids can differ while remaining in the
same template space.

Before combining an atlas and another image, check both:

1.  Do their declared template spaces match?
2.  Do their voxel grids match?

If the template spaces differ, consult the transform plan before
resampling:

``` r

atlas_transform_plan(
  "MNI152NLin6Asym",
  "MNI152NLin2009cAsym",
  data_type = "voxel"
)
#> <atlas_transform_plan>
#>   from_space: MNI152NLin6Asym 
#>   to_space: MNI152NLin2009cAsym 
#>   n_steps: 1 
#>   status: planned 
#>   confidence: high 
#>   warnings: Plan includes unimplemented/planned transform step(s).
```

A plan marked `planned` is not an executable transform. Do not use
ordinary grid resampling to imply nonlinear registration between
templates.

## What is cached?

The pure-R `templateflow` package manages downloaded files on disk.
Within an R session, `neuroatlas` also memoises loading a resolved NIfTI
path into a `NeuroVol`.

``` r

show_templateflow_cache_path()
#> [1] "/home/runner/.cache/R/neuroatlas/templateflow"
```

[`clear_templateflow_cache()`](../reference/clear_templateflow_cache.md)
removes cached assets and in-memory loading state; because that is
destructive and may trigger large downloads later, call it only when you
deliberately want to rebuild the cache.

## Where should you go next?

- Return to
  [`vignette("neuroatlas-overview")`](../articles/neuroatlas-overview.md)
  for the atlas-to-summary workflow.
- Read
  [`vignette("surface-templates")`](../articles/surface-templates.md)
  for surface geometry and data.
- Use
  [`space_transform_manifest()`](../reference/space_transform_manifest.md)
  and [`atlas_transform_plan()`](../reference/atlas_transform_plan.md)
  before mixing images from different template spaces.

For archive naming and governance, consult the [TemplateFlow
documentation](https://www.templateflow.org/).
