# TemplateFlow Space Transforms Deep Dive

Status: historical design note / preprocessing plan.
Captured: 2026-06-18.

2026-10-06: the volume application/cache API and qualified V1 artifacts now
exist. The starting-point descriptions below are historical, not current
blockers. Follow [the current coverage plan](template-transform-coverage.md)
and [machine handoff](resume-neuroatlas.md) for remaining surface/projection work.

## Goal

Provide `neuroatlas` users with reliable mappings between TemplateFlow template
spaces so an atlas or image in one standard space can be moved to another
closely related space, for example `MNI152NLin6Asym` to
`MNI152NLin2009cAsym`.

This should not make `neuroatlas` run registrations at install time or normal
runtime. Transform estimation belongs in a preprocessing/build step. The
package should expose discovery, planning, download/cache, and apply helpers.

## Current neuroatlas surface

`neuroatlas` already has the right front door:

- `R/transform_registry.R` exposes `space_transform_manifest()` and
  `atlas_transform_plan()`.
- `inst/extdata/transform_registry.csv` already records the two MNI variant
  TemplateFlow ANTs routes as `planned`:
  - `MNI152NLin6Asym -> MNI152NLin2009cAsym`
  - `MNI152NLin2009cAsym -> MNI152NLin6Asym`
- `R/alignment_registry.R` uses the space registry when two atlas objects have
  the same family/model/representation but different template spaces.
- `R/coordinate_spaces.R` distinguishes affine coordinate transforms
  (`MNI305 <-> MNI152`) from same-coordinate-system template warps.

So the missing neuroatlas layer is not route planning. It is:

1. resolve a route to a concrete transform artifact,
2. apply the transform to `NeuroVol` / atlas data with correct interpolation,
3. validate and document which routes are genuinely available.

## TemplateFlow inventory check

In the current local R environment, `templateflow::tf_templates()` reports 30
templates. However, `tf_ls(..., as_df = TRUE)` shows only a small transform
graph, not an all-pairs transform set.

Rows with `suffix == "xfm"` observed locally:

- `MNI152NLin2009cAsym`:
  - from `MNI152NLin6Asym`, `.h5`
  - from `OASISTRT20`, `.mat`
- `MNI152NLin6Asym`:
  - from `MNI152NLin2009cAsym`, `.h5` and `.mat`
  - from `MNIInfant+1` through `MNIInfant+11`, `.h5`
- `MNIInfant` cohorts:
  - from `MNI152NLin6Asym`, `.h5`

The key MNI pair can be fetched directly:

```r
templateflow::tf_get(
  template = "MNI152NLin6Asym",
  from = "MNI152NLin2009cAsym",
  mode = "image",
  suffix = "xfm",
  extension = ".h5"
)
```

The downloaded files are large, around 205 MB each. They should not be bundled
inside the CRAN package.

## neurotransform compatibility finding

`neurotransform` is the right R-side apply engine in principle:

- `ants_h5_morphism(path, source, target)`
- `resample_to(moving, target, transform)`
- `compose()`, `invert()`, `read_transform()`

But the current installed `neurotransform::ants_h5_morphism()` does not load
the TemplateFlow MNI `.h5` files. The error is:

```text
An object with name TransformFixedParameters does not exist in this group
```

`h5ls` shows why. The TemplateFlow files contain:

```text
/TransformGroup/1/TranformFixedParameters
/TransformGroup/1/TranformParameters
/TransformGroup/2/TranformFixedParameters
/TransformGroup/2/TranformParameters
```

while the `neurotransform` loader expects:

```text
TransformFixedParameters
TransformParameters
```

The bundled `neurotransform` ANTs sample uses the expected spelling, so
TemplateFlow compatibility needs a small fallback reader that accepts both
spellings.

## miniprep relevance

`miniprep` is subject-pipeline oriented, so it should not become a runtime
dependency for `neuroatlas` template-to-template mappings. Its useful pieces
are design patterns:

- explicit fit/apply split: estimate transforms first, materialize images late,
- typed artifacts with checksums and provenance,
- BIDS-derivatives naming for transforms:
  `from-<source>_to-<target>_mode-image_xfm.h5`,
- transform chain summaries and single-interpolation apply semantics,
- spatial graph concept for route composition.

For this task, reuse those conventions rather than forcing canonical
TemplateFlow transforms through a subject BIDS workflow.

## niflowr relevance

`niflowr` is the right low-level place for reproducible command execution and
provenance:

- spec-driven CLI calls,
- safe argument vectors through `processx`,
- runtime profiles for native/container execution,
- provenance JSON sidecars.

It has bundled ANTs specs for:

- `ants.registration`
- `ants.registration_syn_quick`
- `ants.apply_transforms`
- `ants.compose_multi_transform`

But the current generated ANTs specs are not ready for production use as-is:

- enum values with numeric format strings can render as literal `%d`,
- list args with `%s...` can render literal ellipses on file paths,
- `ants.registration` does not render the fixed/moving/metric/stage arguments
  from the generated spec,
- `ants.registration_syn_quick` has no declared outputs.

Recommendation: either patch/add focused `niflowr` specs first, or use
`niflowr` only as the provenance/runtime layer while a preprocessing script
emits explicit ANTs commands.

## Proposed preprocessing pipeline

Create a build script outside the normal package runtime, probably under
`data-raw/` or a separate transform-artifact repo:

1. Enumerate TemplateFlow templates and official xfm rows with
   `templateflow::tf_ls(as_df = TRUE)`.
2. For every official `.h5` xfm row, download it and write a manifest row with
   source, target, mode, format, size, digest, TemplateFlow relpath, and status.
3. For selected missing edges, run ANTs registration from source template T1w
   to target template T1w.
4. Write outputs using TemplateFlow/BIDS-like names:
   `tpl-<target>_from-<source>_mode-image_xfm.h5`.
5. Emit sidecars with:
   source/target template IDs, fixed/moving image paths and digests, masks,
   ANTs version, full command args, random seed, date, host/container runtime,
   QA metrics, and provenance.
6. Publish or cache artifacts outside the package, and update
   `inst/extdata/transform_registry.csv` with stable relpaths/URLs/digests.

Do not compute a full all-pairs direct graph initially. With 30 TemplateFlow
templates, that would be 870 directed transforms and likely hundreds of GB.
Instead, expose arbitrary source-target mapping through a transform graph:

- use official direct TemplateFlow edges where present,
- support two-hop or multi-hop routes through a small set of adult-MNI hubs,
- only generate direct edges for atlas spaces we actually support.

## Proposed neuroatlas API

Keep the public package lightweight:

- `template_transform_manifest()`:
  return official and neuroatlas-generated transform rows.
- `get_template_transform(from, to, mode = "image", ...)`:
  resolve direct or graph route, fetch/cache artifacts, verify digest.
- `apply_template_transform(x, from, to, interpolation = NULL, data_type = NULL)`:
  apply a route to a `NeuroVol`, using nearest/GenericLabel for labels and
  linear/BSpline for continuous images.
- `map_atlas(atlas, to_space, ...)`:
  atlas-aware wrapper that preserves labels/provenance and repairs contiguous
  cluster IDs when needed.

`atlas_alignment()` and `atlas_transform_plan()` can stay as the advisory layer,
but registry rows should move from `planned` to `available` only after the
actual artifact is fetchable and load/apply tests pass.

## Validation requirements

Minimum checks before marking a route available:

- transform file exists, digest matches registry,
- `neurotransform` can load the `.h5`,
- a fixed template T1w can be transformed onto target grid without error,
- label interpolation preserves integer labels for atlas data,
- round-trip source -> target -> source is sane on masks/landmarks,
- Dice/Jaccard or overlap metrics are recorded for brain masks and a few
  atlas-like label maps,
- no runtime download/registration is required for unit tests.

## Immediate blockers

1. Patch `neurotransform` ANTs H5 loading to accept both
   `TransformParameters` and `TranformParameters` spellings.
2. Decide whether to patch `niflowr` generated ANTs specs or create a small
   focused transform-build spec.
3. Decide artifact hosting: TemplateFlow official cache only, GitHub release
   assets, an external data package, or a private/public object bucket.
4. Scope the first route set. Recommended first slice:
   `MNI152NLin6Asym <-> MNI152NLin2009cAsym`, because these are already used
   in `neuroatlas` Schaefer/TemplateFlow workflows and official `.h5` files
   exist.
