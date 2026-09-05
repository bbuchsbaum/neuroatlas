# Identify, cite, and trace an atlas

An atlas object should tell you which resource you loaded, where its
data came from, which coordinates it uses, and what to cite. That
information stays with the object when you save it. Inspecting it does
not contact a server.

## Inspect a loaded atlas

The bundled ASEG atlas makes a small, offline example:

``` r

a <- get_aseg_atlas()
atlas_metadata(a)
#> <atlas metadata> ASEG
#>   Description:   Bundled standard-space FreeSurfer subcortical labels.
#>   Version:       not recorded
#>   Content:       labels, volume
#>   Regions:       17
#>   Template:      MNI152_unspecified
#>   Coord. space:  MNI152
#>   Space basis:   inferred
#>   Voxel size:    1 x 1 x 1 mm
#>   Source:        bundled_extdata
#>   License:       not recorded
#>   Citation:      [atlas] Bruce Fischl et al. (2002); doi:10.1016/S0896-6273(02)00569-X
#>   Modifications: none recorded
```

The summary separates the atlas’s identity from its current voxel
geometry. `MNI152_unspecified` means that the precise anatomical
template has not been verified. The voxel size and affine can still be
measured from the data. Matching a familiar image’s dimensions is
insufficient evidence to name its registration target.

The complete record has a stable schema:

| Component | Contents |
|:---|:---|
| `identity` | Identifier, name, description, published version, species, coverage |
| `content` | Volume or surface, labels or probabilities, variant parameters, region count |
| `spatial` | Anatomical template, evidence basis, actual grid or mesh geometry |
| `citations` | Bibliographic records with atlas, distribution, template, or software roles |
| `artifacts` | Source files, release identifiers, license information, checksums |
| `history` | Operations, structured parameters, and neuroatlas version |
| `parents` | Metadata snapshots for composite and derived resources |

``` r

m <- atlas_metadata(a)
m$spatial$voxel_size
#> [1] 1 1 1
m$spatial$affine
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0  -96
#> [2,]    0    1    0 -132
#> [3,]    0    0    1  -78
#> [4,]    0    0    0    1
```

Missing information is stored as `NA` and shown as “not recorded”. A
paper’s publication year is not a data release version. Likewise, a
source URL is not proof that downloaded bytes match a particular
release.

## Get the right references

``` r

atlas_citations(a)
#> Fischl B, others (2002). "Whole brain segmentation: automated labeling
#> of neuroanatomical structures in the human brain." _Neuron_.
#> doi:10.1016/S0896-6273(02)00569-X
#> <https://doi.org/10.1016/S0896-6273%2802%2900569-X>. Role: atlas,
#> <https://doi.org/10.1016/S0896-6273(02)00569-X>.
utils::toBibtex(atlas_citations(a))
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

These are ordinary R `bibentry` objects. Use `role = "atlas"` to select
the atlas publications, or inspect the citation table to see each
reference’s role:

``` r

m$citations[, c("role", "title", "doi")]
#> # A tibble: 1 × 3
#>   role  title                                                              doi  
#>   <chr> <chr>                                                              <chr>
#> 1 atlas Whole brain segmentation: automated labeling of neuroanatomical s… 10.1…
```

A projected atlas may require the original atlas publication, the
distributed projection, and the reference template. For example, Glasser
surface metadata include the original Glasser publication and the Mills
Figshare projection, alongside references captured for the loaded
TemplateFlow geometry.

The catalog uses the distributor’s recommended references where
available, including [FSL’s atlas
references](https://fsl.fmrib.ox.ac.uk/fsl/docs/other/datasets.html).
Olsen’s original publication remains explicitly unverified. References
without complete bibliographic fields remain title/URL entries; the
accessor does not guess the missing fields or resolve them online.

## Trace changes to the object

Filtering keeps the original source records and adds a history step with
the retained region identifiers:

``` r

left <- filter_atlas(a, hemi == "left")
atlas_metadata(left)$content$regions
#> [1] 7
atlas_history(left)[, c("step", "action", "details")]
#> # A tibble: 2 × 3
#>    step action details                   
#>   <int> <chr>  <chr>                     
#> 1     1 load   Loaded bundled ASEG atlas.
#> 2     2 subset Kept 7 of 17 ROIs.
tail(atlas_history(left)$parameters, 1)
#> [[1]]
#> [[1]]$keep_ids
#> [1] 10 11 12 13 17 18 26
```

Resampling changes the current grid while preserving the original
artifact’s resolution. Here the atlas is sampled at 2 mm:

``` r

target <- neuroim2::NeuroSpace(
  dim = as.integer(ceiling(dim(a$atlas) / 2)),
  spacing = c(2, 2, 2),
  origin = neuroim2::origin(neuroim2::space(a$atlas))
)
coarse <- get_aseg_atlas(outspace = target)
atlas_ref(coarse)$resolution
#> [1] "2mm"
atlas_artifacts(coarse)$resolution
#> [1] "1mm"
tail(atlas_history(coarse)$parameters, 1)[[1]]$interpolation
#> [1] "nearest"
```

Grid resampling does not perform anatomical registration. The native
template identity is retained; a requested target grid is recorded
separately as `spatial$sampling_reference` and in the resampling
parameters. Supplying a different TemplateFlow name as `outspace` does
not prove that a nonlinear warp to that template was applied.

[`dilate_atlas()`](../reference/dilate_atlas.md) records its radius and
neighbour limit. [`merge_atlases()`](../reference/merge_atlases.md)
keeps both parent metadata records and their references, records the
identifier map and overlap rule, and rejects conflicting grids or known
template identities. Surface subsetting remains unsupported; its
existing diagnostic is unchanged.

## Templates and probability maps

Template volumes and surfaces use the same schema, attached as an
attribute so their `NeuroVol` and `SurfaceGeometry` classes remain
intact:

``` r

t <- get_template("MNI152NLin2009cAsym", resolution = "01")
template_metadata(t)
template_citations(t)

s <- load_surface_template("fsaverage", "pial", hemi = "both", density = "164k")
template_metadata(s)$spatial$vertex_count
template_metadata(s$L)$artifacts
```

These examples require TemplateFlow data and are not run when building
this article. The loader captures the resolved filename and query,
template description, available license and references, and actual
geometry. A query’s resolution key, such as `"01"`, is retained as a
parameter; voxel sizes come from the image rather than interpreting that
key as millimetres. `TemplateFlowVersion` is retained separately in
provenance; it is not treated as the original template’s published
version.

`get_wang_prob_atlas(path_only = FALSE)` also provides
[`atlas_metadata()`](../reference/atlas_metadata.md) and
[`atlas_citations()`](../reference/atlas_citations.md). Its record
distinguishes probability maps from maximum-probability labels, and
retains the individual volume records in `parents`. Path-only requests
retain their existing path/manifest return types.

## Save the evidence with your analysis

``` r

saveRDS(left, "left-aseg.rds")
restored <- readRDS("left-aseg.rds")
atlas_metadata(restored)
```

Artifact checksums describe source bytes, not the current in-memory
atlas after modifications. R versions with
[`tools::sha256sum()`](https://rdrr.io/r/tools/sha256sum.html) record
SHA-256; older R versions record a labelled MD5 checksum. For installed
bundled datasets, `checksum_basis = "source_manifest"` identifies the
checksum of the source `.rda` file, which R may have converted to its
lazy-load database. `read_file` identifies a file hashed at load time. A
recorded checksum is a content identity, not an assertion that an
upstream publisher supplied a matching checksum.

The metadata record is authoritative. Existing
[`atlas_ref()`](../reference/atlas_ref.md),
[`atlas_artifacts()`](../reference/atlas_artifacts.md), and
[`atlas_history()`](../reference/atlas_history.md) calls continue to
work for atlas objects. Direct edits to legacy compatibility fields do
not update the record. Old saved objects receive conservative metadata
views with unknown provenance. Preservation is supported for
neuroatlas’s loaders and atlas operations; arbitrary data replacement or
transformations in other packages may require their own provenance
handling.
