# Advisory complexity review

Reviewed 2026-10-06 for the 0.2.0 slice. The owner selected neuroatlas conventions
for the lint gate and separate complexity review. The gate has no complexity
ceiling. This report identifies review priorities without requiring a broad
refactor or changing numerical acceptance thresholds.

The installed `lintr::cyclocomp_linter(complexity_limit = 15L)` reports 105
functions above that advisory threshold across `R/`. The largest scores are:

| Function | File | Score |
|---|---|---:|
| `plot_brain()` | `R/plot_brain.R` | 382 |
| `plot_brain_grid()` | `R/plot_brain_grid.R` | 114 |
| `validate_resource_metadata()` | `R/atlas_metadata.R` | 109 |
| `.validate_surface_transform()` | `R/surface_transform.R` | 108 |
| `apply_template_transform()` | `R/template_transform.R` | 98 |
| `.cluster_explorer_server()` | `R/cluster_explorer.R` | 95 |
| `apply_surface_projection()` | `R/surface_projection.R` | 80 |
| `atlas_transform_plan()` | `R/transform_registry.R` | 67 |
| `.read_cbig_ras()` | `R/surface_projection.R` | 53 |
| `surface_data()` | `R/surface_transform.R` | 52 |
| `.depth_cull_faces()` | `R/plot_brain.R` | 51 |
| `validate_parcel_data()` | `R/parcel_data.R` | 50 |

Scores include branches in validation predicates. A high score does not establish
a defect, and replacing explicit integrity guards solely to reduce it can make
the code harder to assess. The surface validator and locked MAT reader should
retain their rejection checks and independent evidence.

Prioritize a separate extraction of presentation/layout helpers from
`plot_brain()` and `plot_brain_grid()`. Preserve existing CPU polygon, occlusion,
hemisphere, orientation and layout regressions. Review the explorer server's
reactive state transitions independently of rendering. For transform functions,
consider separating input validation, data-kind dispatch and coverage reporting
only when it makes their policy boundaries clearer. Keep scalar, probability,
categorical, missingness and staged-composition evidence unchanged.

The current release implements exact identity admission, directed native routes,
and population/ribbon projection with their tests and frozen qualification.
This complexity review makes no anatomical, inverse or conservation claim and
introduces no runtime dependencies.

Reproduce the advisory scan with development tools installed:

```r
lintr::lint_dir(
  "R",
  linters = list(complexity = lintr::cyclocomp_linter(15L))
)
```

The raw devbox report is retained in
`data-raw/surface-transforms-v1/work/complexity-review.json`; that local work
directory is excluded from Git and package builds. The project gate is reproduced
separately with `Rscript .github/scripts/lint.R`.
