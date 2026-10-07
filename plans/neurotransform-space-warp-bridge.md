# Note: cross-space volume warps via neurotransform

Status: historical idea note. Captured 2026-06-04.

2026-10-06: volume transform discovery, verified caching and application are
implemented with qualified V1 artifacts. The gap descriptions below record the
June starting point. Follow [the current coverage plan](template-transform-coverage.md)
and [machine handoff](resume-neuroatlas.md) for current work and limitations.

Excluded from CRAN build — `plans/` is in `.Rbuildignore`.

## Problem

Users want to take a volume in one standard space (e.g. `MNI152NLin2009cAsym`)
and display/overlay it on a *different* volumetric template
(e.g. `MNI152NLin6Asym`). That requires a true nonlinear inter-template warp.

## What we have today

- `R/coordinate_spaces.R`: exact prebuilt **affine** MNI305 <-> MNI152
  (`MNI305_to_MNI152`, `get_space_transform()`, `transform_coords()`,
  `transform_vertices_to_volume()`). Coordinates/vertices only, this one pair only.
- `R/transform_registry.R` + `inst/extdata/transform_registry.csv`: a route
  **registry + planner** (`space_transform_manifest()`, `atlas_transform_plan()`).
  It *names* the warp we'd need —
  `from-MNI152NLin6Asym_to-MNI152NLin2009cAsym_mode-image_xfm.h5` (ANTs `.h5`) —
  but the MNI-variant volumetric routes are `status = planned` (not executable).
- `resample()` (wraps `neuroim2::resample`): grid reslicing only, assumes same
  coord space. NOT a nonlinear inter-template warp.
- `tflow_files()`: can already **fetch** arbitrary TemplateFlow files, including
  the `..._xfm.h5` composite warps.

Gap: nothing **loads or applies** a volumetric warp.

## The missing piece: neurotransform (~/code/neurotransform)

Sibling package "Geometric Transforms for Neuroimaging Data". It is exactly the
apply-engine the registry was scaffolding toward. Relevant exports:

- `ants_h5_morphism(path, source, target)` — loads an ANTs composite `.h5`
  (the exact file type our registry references) into a `Warp3DMorphism` /
  `[warp, affine]` `MorphismPath` with correct pullback ordering.
- `resample_to(moving, target, transform)` — applies a morphism to a `NeuroVol`,
  returns a `neuroim2` volume on the target grid. Also `resample_volume()`.
- `compose()`, `invert()`, `jacobian()`; IO for ANTs/FSL/AFNI/X5/dense fields.
- Light deps (Rcpp/RcppArmadillo/neuroim2). GitHub: bbuchsbaum/neurotransform.

## Proposed bridge (in neuroatlas, neurotransform in Suggests)

Thin wrapper `apply_space_transform(vol, from, to, ...)` that:
1. `atlas_transform_plan(from, to)` to resolve the route from the registry.
2. `tflow_files()` to fetch the route's `.h5` (data_files column).
3. `neurotransform::ants_h5_morphism()` to load it.
4. `neurotransform::resample_to(vol, target = get_template(to), morph)`.
5. Flip the corresponding `transform_registry.csv` rows `planned` -> `available`.

Watch-outs:
- ANTs directionality: files are named by the *forward* map; resampling needs
  the *pullback*. `ants_h5_morphism` handles `[warp, affine]` pullback ordering;
  use `invert()` if a given `.h5` is oriented the other way. Verify per route.
- Keep neurotransform in `Suggests` + guard with `requireNamespace()` (it's not
  on CRAN yet) so package build/check stays clean.

## End-to-end recipe (works today, manually)

```r
neuroatlas::atlas_transform_plan("MNI152NLin2009cAsym", "MNI152NLin6Asym")
xfm <- neuroatlas::tflow_files(
  "MNI152NLin6Asym",
  query_args = list(from = "MNI152NLin2009cAsym", mode = "image",
                    suffix = "xfm", extension = ".h5"))
target <- neuroatlas::get_template("MNI152NLin6Asym")
morph  <- neurotransform::ants_h5_morphism(
  xfm, source = "MNI152NLin2009cAsym", target = "MNI152NLin6Asym")
moved  <- neurotransform::resample_to(my_vol, target = target, transform = morph)
```
