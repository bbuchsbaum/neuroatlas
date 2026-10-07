# CIFTI adapter development evidence

This qualifies the development adapter after neuroatlas 0.2.0. It does not
change the published release or its frozen numerical receipts.

## Scope and numerical evidence

[The frozen contract](../../contract-v1.json) requires exact transport and
adapter agreement. [Source bindings](source-bindings.json) and the
[consumer receipt](adapter-consumer-receipt.json) bind the measured sources,
protocol and installed pinned engine. Rebuild and bind the engine on another
machine; do not borrow this machine's compiled-artifact hash.

- [Independent I/O oracle](io-oracle-receipt.json): 17 synthetic NiBabel files,
  scalars/labels, both axis orders, one/two maps, little/big endian and scaling.
  Values, missing scalars, axes, brain models, volume geometry, map metadata,
  label keys/names/RGBA and other extensions agree exactly after R transport.
- [Full-density consumer](adapter-consumer-receipt.json): eight mixed cases,
  fsaverage 164k to fsLR 32k and reverse, L/R, scalar/label, both axis orders.
  2,874,376 values agree exactly with direct public surface application, with
  zero availability mismatches. Reordered thalamic voxels retain their values;
  explicit missing label keys retain unavailable rows after writing/reading.
- Independent NiBabel verification of written
  [downsampled](adapter-down-format-receipt.json) and
  [upsampled](adapter-up-format-receipt.json) outputs passes for values, brain
  axes, map names/metadata, label tables and extra extensions.
- [Workbench inspection](workbench-inspection-receipt.json) accepts all 17 files
  for brain-model inspection. Transposed axes are classified as an unknown or
  unsupported subtype; standard dense scalar/label ordering is recognized.

The adapter preserves subcortex on unchanged geometry and support in a
caller-declared common exact MNI6/MNI2009c frame. It rejects changed voxel
geometry, support, structure inventory or declared frame. It does not implement
subcortical warping, surface-to-volume rasterization, other densities or CIVET.
The domain binding is explicit and caller-declared: vertex counts and affine
equality cannot prove registration/template identity.

Missing labels require explicit existing keys. Neuroatlas records unavailable
brainordinates in per-map metadata and honors them when rereading. External
readers may only interpret the selected key; zero itself proves no missingness.
These checks qualify the adapter, not a new numerical method, Workbench
resampling equivalence, held-out anatomical accuracy, conservation or an inverse.

## Retained failures

The I/O attempt [02](cifti-io-oracle-02.log) exposed a genuine singleton-axis
writing defect: RNifti trimmed a trailing unit dimension. The final writer
retains both CIFTI matrix dimensions; [03](cifti-io-oracle-03.log) passes without
changing the exact-value contract.

Adapter attempt [01](cifti-adapter-01.log) selected the wrong offline cache;
[02](cifti-adapter-02.log) resolved the virtual-environment Python symlink and
lost its packages; [03](cifti-adapter-03.log) passed the down direction but lacked
reverse operators in that cache. The driver now retains the virtual-environment
path, explicitly selects the verified cache, builds missing native operators,
and checks their exact offline replay. Final attempt
[04](cifti-adapter-04.log) passes every case. These failures remain retained.

## Engineering

[Project lint](cifti-lint-04.log) passes with zero findings. Full development
tests, package/manual checks and the website build are recorded separately when
complete. Refer to the current branch/PR for publication and hosted CI status.
