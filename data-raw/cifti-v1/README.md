# CIFTI cortical adapter qualification

This development slice adds CIFTI-2 scalar/label transport and cortical
resampling through existing qualified surface operators. It preserves
noncortical values on unchanged support in an explicitly declared common exact
MNI frame. It does not warp subcortex, rasterize surfaces, add densities, or
establish an inverse, conservation, anatomical accuracy or Workbench numerical
equivalence.

RNifti and optional xml2 are runtime dependencies. NiBabel and Workbench are
independent qualification tools. Freeze `contract-v1.json` before measurements.
Retain each attempt under a separate local output directory; do not overwrite
failed evidence. Generated NIfTI files, fields and operator caches stay local.

From the repository with its pinned engine/library environment loaded:

```sh
python data-raw/cifti-v1/io-oracle.py generate /path/to/new-io-attempt
Rscript data-raw/cifti-v1/roundtrip.R /path/to/new-io-attempt
python data-raw/cifti-v1/io-oracle.py verify /path/to/new-io-attempt
python data-raw/cifti-v1/inspect-workbench.py /path/to/new-io-attempt
Rscript data-raw/cifti-v1/qualify-adapter.R /path/to/new-adapter-attempt \
  /absolute/path/to/venv/bin/python /path/to/verified-surface-cache
```

The I/O oracle independently generates 17 files: scalar/label, both brain-axis
orders, one/two maps, little/big endian, and a scaled NIfTI case. It checks
values, axes, brain models, metadata, label keys/names/RGBA and extra extensions
exactly through NiBabel. The adapter driver checks both hemispheres in each
fsaverage 164k/fsLR 32k direction, two-map scalars/labels, both axis orders,
reordered models and voxel rows, missing cortical samples and explicit missing
label keys. Results must exactly match direct public surface application;
availability and noncortical values must also match exactly. NiBabel checks the
written adapter outputs independently.

Workbench can inspect both axis orders but classifies the transposed ordering
as an unknown/unsupported CIFTI subtype. Standard dense scalar/label ordering
uses the brain-model axis as dimension 1. Transport preserves the original
ordering rather than silently transposing it. Availability for explicitly
unassigned labels uses `neuroatlas.unavailable_brainordinates` in per-map
metadata: neuroatlas honors it, while other readers may only see the declared
label key. Label zero alone does not establish missingness or sampled support.

A typical public workflow binds domains explicitly:

```r
source <- read_cifti("source.dlabel.nii")
reference <- read_cifti("target-layout.dlabel.nii")
left <- get_template_transform(source_left_geometry, target_left_geometry)
right <- get_template_transform(source_right_geometry, target_right_geometry)
operator <- get_template_transform(source, reference,
  cortex = list(L = left, R = right), volume_space = "MNI152NLin6Asym")
mapped <- apply_template_transform(source, operator, missing_labels = 0)
write_cifti(mapped, "mapped.dlabel.nii")
```

The caller must verify that the file's cortical ordering matches those exact
domains. Vertex counts and matching voxel affines cannot establish registration
or template identity. An explicit missing key must already occur in every
map's own label table. Source map metadata and per-map tables survive the
operation; the reference contributes its brain-model layout only.
