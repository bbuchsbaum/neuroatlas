# CIFTI mixed workflows

Implementation branch: `feat/cifti-mixed-transforms`, after released 0.2.0.
Tracker: `bd-01M3CCBG9346NRV3WEVH5YNBCQ`.

The owner selected existing RNifti plus optional xml2 for file I/O. CIFTI is a
container, not a template identifier. This slice reads and writes CIFTI-2 dense
scalar and label maps, and applies the existing qualified cortical operators
while preserving volumetric structures on their original grid. A changed
subcortical grid or frame requires a separately qualified volumetric operation;
this first adapter must reject it rather than drop or invent structures.

Use the package's existing S3 list conventions: a `CiftiData` holds a numeric
matrix with brainordinates in rows and maps in columns, the original matrix-axis
layout, ordered brain models and their zero-based vertex/voxel indices, volume
geometry, map names, separate label tables for each map, and original XML and
NIfTI metadata. File metadata cannot establish surface registration or exact MNI
identity; cortical execution requires explicitly supplied verified geometries.
Do not infer domain bindings from vertex counts or density names.

The first mixed transform changes cortex only. Its target brain-model layout
must retain the same noncortical structures and voxel coordinates. Match those
rows by structure and voxel index, not position, so reordered models remain
correct. Explicitly declare the common MNI6/MNI2009c volume frame; matching
affines cannot establish template identity. Keep source map names, metadata
and label tables. Missing cortical data
remains missing; never silently assign label zero. Label-file writing must reject
missing values unless an explicit declared unassigned label is selected.

Freeze acceptance before implementation measurements: transport round trips
preserve finite values, missing scalar values, brain-model indices/order,
structures, affine/units, map metadata and label keys/names/RGBA exactly (numeric
values as doubles). XML formatting need not be byte-identical. Both matrix-axis
layouts, sparse/reordered vertices, asymmetric hemispheres and voxel indices,
multiple map-specific label tables, negative keys and reserved zero are tested.
Reject overlapping/gapped ranges, duplicates, out-of-range indices, unknown
mapping types, dimension/intent mismatches, missing volume geometry, invalid label
tables, unsupported noncortical changes and XML entity declarations.

Use independent NiBabel-generated synthetic files for I/O verification and
Workbench file inspection. Python/Workbench are qualification tools, not runtime
requirements. Cortical values must match the existing public surface application
on the same explicitly bound data and geometry; this checks the adapter, not a
new numerical method or anatomical claim. Additional densities and directed
rasterization remain separate work.

Sources: [CIFTI-2 specification](https://www.nitrc.org/forum/attachment.php?attachid=333&forum_id=1955&group_id=454),
[XML appendix](https://www.nitrc.org/forum/attachment.php?attachid=334&forum_id=1955&group_id=454),
[RNifti](https://github.com/jonclayden/RNifti), and
[NiBabel CIFTI](https://nipy.org/nibabel/reference/nibabel.cifti2.html).
