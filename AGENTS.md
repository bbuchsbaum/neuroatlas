# neuroatlas

## Purpose

R package providing a unified framework for neuroimaging atlases and
parcellations. Wraps volumetric and surface-based brain atlases
(Schaefer, Glasser, FreeSurfer ASEG, Olsen MTL) in a consistent S3 class
system with generics for ROI extraction, data reduction, region
subsetting, and visualization.

## Key Files

| File           | Description                                                                |
|----------------|----------------------------------------------------------------------------|
| `DESCRIPTION`  | Package metadata, version, and dependency declarations                     |
| `NAMESPACE`    | Auto-generated exports/imports (managed by roxygen2 — never edit manually) |
| `CLAUDE.md`    | Development instructions for Claude Code                                   |
| `_pkgdown.yml` | pkgdown site configuration                                                 |
| `README.Rmd`   | Package README source (renders to README.md)                               |
| `NEWS.md`      | Changelog                                                                  |

## Subdirectories

| Directory         | Purpose                                                               |
|-------------------|-----------------------------------------------------------------------|
| `R/`              | All R source code — S3 classes, generics, methods (see `R/AGENTS.md`) |
| `tests/testthat/` | testthat test suite (see `tests/testthat/AGENTS.md`)                  |
| `data/`           | Precomputed `.rda` datasets bundled with the package                  |
| `data-raw/`       | Scripts and raw files used to generate `data/` contents               |
| `inst/extdata/`   | NIfTI atlas volumes shipped with the package                          |
| `vignettes/`      | Package vignettes and tutorials                                       |
| `man/`            | Auto-generated roxygen2 documentation (do not edit)                   |
| `docs/`           | Auto-generated pkgdown website (do not edit)                          |

## For AI Agents

### Working In This Directory

- Follow tidyverse style: 2-space indentation, no tabs, ~80 char lines,
  `snake_case` functions
- Use `UpperCamelCase` for S3/S4 classes
- All exports require roxygen2 documentation; run `devtools::document()`
  after changes
- Use `assertthat` for input validation
- Check optional packages with
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) before use
- The NAMESPACE file is auto-generated — never edit it manually
- Make minimal, well-scoped changes consistent with existing patterns
- Avoid introducing new dependencies or architectural styles without
  discussion

### Testing Requirements

- Run `devtools::test()` for the full suite, or
  [`testthat::test_file()`](https://testthat.r-lib.org/reference/test_file.html)
  for individual files
- Tests must `devtools::load_all()` first (or use `devtools::test()`
  which does this)
- Use `skip_on_cran()` for network-dependent or slow tests
- Use `make_toy_*()` helpers for synthetic data — avoid requiring atlas
  downloads in unit tests
- Add tests with every feature or bug fix

### Build and Check

- `devtools::check()` or `R CMD check --as-cran` for CRAN compliance
- No non-ASCII characters in code or data
- Examples must run in \< 5 seconds (use `\dontrun{}` or `\donttest{}`
  for slow ones)
- CI tracks coverage via Codecov

### Commit Guidelines

- Concise, present-tense commit messages (e.g., “Add Schaefer atlas
  loader”)
- Group related changes; don’t mix refactors with behavioral changes
- Ensure `devtools::check()` and `devtools::test()` pass before
  committing

### Key Architecture

- All atlases are S3 lists inheriting from `"atlas"` with fields:
  `$atlas`, `$ids`, `$labels`, `$orig_labels`, `$hemi`, `$name`, `$cmap`
- `$atlas` is either a `ClusteredNeuroVol` or plain `NeuroVol` from
  neuroim2
- Surface atlases add `"surfatlas"` class and `$lh_atlas`/`$rh_atlas`
  fields
- Cluster IDs must be contiguous 1:K when constructing
  `ClusteredNeuroVec` objects
- `.subset_atlas()` is the internal workhorse for subsetting;
  [`sub_atlas()`](reference/sub_atlas.md) and
  [`filter_atlas()`](reference/filter_atlas.md) are public wrappers

## Dependencies

### Internal (sibling packages)

- `neuroim2` — Core neuroimaging data structures (NeuroVol, NeuroSpace,
  ClusteredNeuroVol, ClusteredNeuroVec)
- `neurosurf` — Surface-based operations and FreeSurfer annotation
  reading

### External

- `reticulate` — Python bridge for TemplateFlow
- `ggseg` / `ggsegSchaefer` / `ggsegGlasser` — Brain visualization
- `tibble`, `dplyr`, `tidyr` — Data wrangling
- `rlang` — Tidy evaluation (used in `filter_atlas`)
- `assertthat` — Input validation
- `memoise` — Function memoization for cached atlas loading
