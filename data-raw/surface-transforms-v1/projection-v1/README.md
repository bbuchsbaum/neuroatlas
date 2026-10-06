# Population cortical projection qualification

This protocol covers the exact MNI152NLin6Asym and MNI152NLin2009cAsym frames
on four locked 1 mm / 2 mm grids, projecting to pinned fsaverage 164k and fsLR
32k domains in each hemisphere. It also tests separately declared, aligned
white/pial ribbon sampling on synthetic oblique grids with 3, 5 and 9 nodes.
It does not qualify arbitrary MNI aliases or fit subject registration.

`contract-v1.json` froze acceptance before candidate execution. Earlier native
and Workbench gates remain unchanged, including their retained failures.
`grids.lock.json` records the exact template brain-mask headers used to define
the tested volume grids. Continuous maps use trilinear interpolation; labels use
nearest voxel centers with lower-index half-voxel ties and categorical surface
votes. Probability channels retain partial mass. Unsupported samples remain
missing, while supported zero is valid.

The original CBIG RF-ANTs coordinates are fetched from revision
`634f676630929a71297852d01dd92a287103e861` with byte checksums. Their target
ordering is checked against that revision's fsaverage spheres using identical
ordered triangles and zero nearest-vertex index mismatches. The sphere coordinates
are not byte-identical; the measured coordinate differences are descriptive.
The CBIG FSL original and TemplateFlow MNI6 reference images have matching
canonical grids and identical sampled cortical support, but differ elsewhere.
No whole-volume equivalence is inferred from this audit.

For a 2009c source, the qualified image pullback maps the MNI6 sampling points
into 2009c physical coordinates, avoiding an intermediate resampled image.
fsLR output additionally applies the separately admitted native surface operator.
Neither composition establishes anatomical accuracy, area conservation or an
inverse mapping. Ribbon alignment is explicitly asserted by its caller, and its
equal-node method differs from Workbench's voxel-intersection ribbon mapping.

## Reproduce

First follow [`plans/resume-neuroatlas.md`](../../../plans/resume-neuroatlas.md)
to rebuild the pinned engine, verify its machine-specific binding receipt and
fetch the surface inputs. Original CBIG reference volumes and target-ordering
spheres are additional audit inputs; their immutable URLs and checksums are
recorded in the package projection manifest and reference driver. Fetch these
from their original providers into `work/archives`; do not bundle them into the
package. Review [`LICENSES.md`](../LICENSES.md).

The devbox uses Python NumPy 2.4.3, SciPy 1.18.1, nibabel 5.4.2 and SimpleITK
2.5.3 for independent references, plus Workbench 1.5.0. Matplotlib 3.11.2 is only
needed for local QA figures. Python is not needed by the public R API.
Every output destination must be new. Serialize jobs sharing the same cache.

```sh
Rscript data-raw/surface-transforms-v1/projection-v1/qualify.R \
  data-raw/surface-transforms-v1/work/projection-new
data-raw/surface-transforms-v1/work/python/bin/python \
  data-raw/surface-transforms-v1/projection-v1/oracle.py \
  data-raw/surface-transforms-v1/work/projection-new
data-raw/surface-transforms-v1/work/python/bin/python \
  data-raw/surface-transforms-v1/projection-v1/references.py \
  data-raw/surface-transforms-v1/work/projection-new
data-raw/surface-transforms-v1/work/python/bin/python \
  data-raw/surface-transforms-v1/projection-v1/test-evidence.py \
  data-raw/surface-transforms-v1/work/projection-new
```

The R driver captures and rechecks its package sources, registry, input manifests
and driver identity; do not edit those files during execution. It checks online
construction against offline replay. The independent oracle verifies all consumed
bytes and compares 108 cases, using SciPy interpolation and independent sparse
accumulation. The reference driver verifies original CBIG coordinates, SimpleITK
pullbacks and Workbench point sampling. Tamper tests reject altered manifests,
coordinates, masks, values and source volume bytes.

For actual Harvard-Oxford labels, both templates' T1w images, per-label cortical
areas and local surface/source-image QA views:

```sh
Rscript data-raw/surface-transforms-v1/projection-v1/public-examples.R \
  data-raw/surface-transforms-v1/work/public-examples-new
data-raw/surface-transforms-v1/work/python/bin/python \
  data-raw/surface-transforms-v1/projection-v1/plot-public-examples.py \
  data-raw/surface-transforms-v1/work/public-examples-new
```

Retain small hash-bound receipts and logs in the tracked qualification directory.
Large arrays, downloads, caches and geometry-derived QA images stay local; sharing
them requires their upstream distribution terms. Report AI and human inspection
separately. The atlas examples are dependent population references, not held-out
anatomical validation. Per-label area changes are descriptive because native
closest-point resampling does not promise area conservation.
