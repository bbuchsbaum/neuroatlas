# Surface Parcellations with neurosurf

## What this vignette covers

This guide focuses on loading **surface parcellations** as actual meshes
(`neurosurf::LabeledNeuroSurface`)—not ggseg flats. We cover:

- Discovering available surface templates in TemplateFlow
- Schaefer on fsaverage6 (packaged geometry)
- Schaefer on fsaverage5/fsaverage (via TemplateFlow, when available)
- Glasser MMP1.0 (via TemplateFlow, when available)
- Loading raw surface geometry from TemplateFlow
- Downloading surfaces to a local folder
- What files are fetched, what the `data` slot contains, and how labels
  map.
- Minimal patterns to attach your own per-vertex values.

## Discovering available surface templates

Before loading surfaces, you can query what’s available in TemplateFlow:

``` r
# Find surface-related template spaces
tflow_spaces(pattern = "fs")
#> [1] "fsLR"      "fsaverage"

# List available fsLR surfaces
tflow_files("fsLR", query_args = list(hemi = "L"))

# Filter by specific density and surface type
tflow_files("fsLR", query_args = list(
  hemi = "L",
  density = "32k",
  suffix = "midthickness"
))

# List fsaverage surfaces at a specific resolution (e.g., fsaverage6)
tflow_files("fsaverage", query_args = list(
  hemi = "L",
  resolution = "06"  # fsaverage6
))
```

Common query parameters for surfaces: - `hemi`: “L” or “R” - `density`:
“32k”, “164k” (for fsLR) - `resolution`: “06” (fsaverage6), “05”
(fsaverage5) - `suffix`: “pial”, “white”, “inflated”, “midthickness”,
“sphere”

## Schaefer surface atlases

``` r
# 200 parcels, 7 networks, fsaverage6 inflated (packaged geometry, no TF needed)
atl <- schaefer_surf(
  parcels  = 200,
  networks = 7,
  space    = "fsaverage6",
  surf     = "inflated"
)
```

Returned structure:

- `atl$lh_atlas`, `atl$rh_atlas`: `LabeledNeuroSurface` objects.
- `slot(..., "data")`: integer parcel IDs per vertex.
- `atl$labels` / `atl$orig_labels`: name lookups; `atl$network`: network
  per ID.

### Using TemplateFlow spaces (when available)

Note: TemplateFlow surface support requires the `fsaverage` template to
be available in your TemplateFlow installation. As of this writing,
surface templates may have limited availability. Use `fsaverage6`
(above) for reliable access to Schaefer atlases.

``` r
# fsaverage5 white surface; uses TemplateFlow geometry (if available)
atl_tf <- schaefer_surf(
  parcels  = 400,
  networks = 17,
  space    = "fsaverage5",
  surf     = "white"
)
```

If TemplateFlow surfaces are available, this uses the same labels/data
layout as above.

### Attaching your own values

``` r
vals <- rnorm(length(atl$ids))        # one value per parcel
mapped <- map_atlas(atl, vals)        # returns LabeledNeuroSurface per hemi with data filled
```

[`map_atlas()`](../reference/map_atlas.md) writes your parcel values
into the `data` slot of each hemisphere surface.

## Glasser MMP1.0 surface atlas (when available)

Note: Like Schaefer TemplateFlow surfaces, Glasser surface support
requires the `fsaverage` template in TemplateFlow. Check availability
before using.

``` r
# Requires fsaverage template in TemplateFlow
glas <- glasser_surf(space = "fsaverage", surf = "pial")

slot(glas$lh_atlas, "data")[1:5]  # parcel IDs
glas$labels[1:5]                  # names for those IDs
```

If available, geometry comes from TemplateFlow (fsaverage
pial/white/inflated/midthickness); labels come from the HCP-MMP1.0
fsaverage annotations published on Figshare as the “HCP-MMP1_0 projected
on fsaverage” dataset (ID 3498446; DOI: 10.6084/m9.figshare.3498446).
The `data` vector length equals the vertex count of the mesh; each entry
is the parcel ID at that vertex.

## What’s in the `data` slot?

- Schaefer / Glasser surface atlases: integer parcel IDs per vertex.
- If you construct a `NeuroSurface` yourself (e.g., from
  [`load_surface_template()`](../reference/load_surface_template.md)),
  you supply the per-vertex numeric values.

## Saving / reusing

``` r
saveRDS(atl, "schaefer200_7_fs6_inflated.rds")
# reload later
atl2 <- readRDS("schaefer200_7_fs6_inflated.rds")
```

### Sanity check: parcel surface renders

We snapshot a labeled Schaefer surface to verify geometry+labels load.
Runs only when TemplateFlow is available (for non-packaged spaces).

``` r
dir.create("figures", showWarnings = FALSE)
png_path <- file.path("figures", "schaefer200_fs6_L_inflated.png")

atl <- schaefer_surf(
  parcels  = 200,
  networks = 7,
  space    = "fsaverage6",  # packaged geometry (no TF), keeps it fast
  surf     = "inflated"
)
#> downloading: https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI//freeview_lut/Schaefer2018_200Parcels_7Networks_order.txt

geom_l <- neurosurf::geometry(atl$lh_atlas)
neurosurf::snapshot_surface(geom_l, file = png_path)
#> Warning in min(x): no non-missing arguments to min; returning Inf
#> Warning in max(x): no non-missing arguments to max; returning -Inf
#> Warning in neurosurf::snapshot_surface(geom_l, file = png_path):
#> rgl.useNULL=TRUE and webshot2 not installed; snapshot may be blank in headless
#> builds.
png_path
#> [1] "figures/schaefer200_fs6_L_inflated.png"
```

![Schaefer 200x7 fsaverage6 inflated left hemisphere labeled
surface](figures/schaefer200_fs6_L_inflated.png)

If the image renders, the parcellated surface is displayable.

## Loading raw surface geometry from TemplateFlow

To load just the surface geometry (without parcellation labels), use
[`get_surface_template()`](../reference/get_template.md) or
[`load_surface_template()`](../reference/load_surface_template.md):

``` r
# Get path to a surface file
fslr_path <- get_surface_template(
  template_id = "fsLR",
  surface_type = "midthickness",
  hemi = "L",
  density = "32k"
)
print(fslr_path)

# Load as a neurosurf SurfaceGeometry object
fslr_geom <- load_surface_template(
  template_id = "fsLR",
  surface_type = "inflated",
  hemi = "L",
  density = "32k"
)

# Load both hemispheres at once
both_hemi <- load_surface_template(
  template_id = "fsLR",
  surface_type = "pial",
  hemi = "both",
  density = "32k"
)
# Returns list with $L and $R
```

## Downloading surfaces to a local folder

To copy TemplateFlow surfaces to a local directory:

``` r
# Get paths to surfaces (automatically downloaded to cache)
files <- tflow_files("fsLR", query_args = list(hemi = "L", density = "32k"))

# Copy to local folder
dest_folder <- "~/my_surfaces/fsLR"
dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
file.copy(files, dest_folder)

# Or use a helper function for bulk downloads
download_surfaces <- function(space, query_args, dest_folder) {
  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
  files <- tflow_files(space, query_args = query_args)
  if (length(files) > 0) {
    dest_paths <- file.path(dest_folder, basename(files))
    file.copy(files, dest_paths, overwrite = TRUE)
    message("Copied ", length(files), " files to ", dest_folder)
  }
  invisible(dest_paths)
}

# Download all fsLR 32k surfaces
download_surfaces("fsLR", list(density = "32k"), "~/my_surfaces/fsLR_32k")
```

## Common workflows

- **Vertex-wise stats:** build your own `NeuroSurface` from
  [`load_surface_template()`](../reference/load_surface_template.md) and
  write stats into `data`.
- **Parcel summaries:** use volumetric data with
  [`reduce_atlas()`](../reference/reduce_atlas.md) or surface atlases
  with [`map_atlas()`](../reference/map_atlas.md) to project parcel
  statistics back onto the mesh for visualization.

## Notes on dependencies

- fsaverage6 geometry is bundled; other spaces require TemplateFlow.
- Chunk `eval = FALSE` by default; set to `TRUE` locally with network
  and TemplateFlow.
