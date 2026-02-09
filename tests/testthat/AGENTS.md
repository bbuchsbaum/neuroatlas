<!-- Parent: ../../AGENTS.md -->
<!-- Generated: 2026-02-08 | Updated: 2026-02-08 -->

# tests/testthat/

## Purpose
testthat (edition 3) test suite for the neuroatlas package. Tests cover atlas loading, data integrity, spatial operations, ROI extraction, data reduction, visualization helpers, and subsetting.

## Key Files

| File | Description |
|------|-------------|
| `test-atlas-integrity.R` | Schaefer atlas loading, label/ID consistency, resampling, merging |
| `test-data-integrity.R` | Data format validation and structural checks |
| `test-sub-atlas.R` | `sub_atlas()` — subsetting by ID, label, hemisphere; both ClusteredNeuroVol and NeuroVol atlases |
| `test-reduce-atlas-vec.R` | `reduce_atlas_vec()` — 4D data reduction to ClusteredNeuroVec with toy data |
| `test-reduce-atlas.R` | `reduce_atlas()` — tibble-based regional summaries |
| `test-dilation.R` | `dilate_atlas()` — parcel boundary expansion |
| `test-merge-atlases.R` | `merge_atlases()` — ID conflict handling, referential integrity |
| `test-roi-metadata.R` | `roi_metadata()`, `roi_attributes()`, `filter_atlas()` |
| `test-roi-operations.R` | `get_roi()` — region extraction by label/ID/hemisphere |
| `test-roi-colors.R` | Color generation algorithms |
| `test-coordinate-spaces.R` | MNI152/MNI305 transforms |
| `test-space-transforms.R` | Atlas resampling, outspace resolution, cross-space operations |
| `test-subcortical-atlases.R` | Subcortical atlas loading and structure |
| `test-surface-atlases.R` | Surface-based atlas loading (Schaefer/Glasser surfatlas) |
| `test-templateflow-advanced.R` | TemplateFlow Python bridge integration |

## For AI Agents

### Working In This Directory
- Test files are named `test-{topic}.R` mirroring functionality in `R/`
- Each test file is self-contained with its own helpers at the top
- Use `make_toy_*()` helper functions for synthetic test data (no network I/O)
- Always wrap tests requiring network access or real atlas downloads with `skip_on_cran()`

### Testing Patterns

**Toy data helpers** (defined at top of each test file):
```r
make_toy_atlas()          # ClusteredNeuroVol atlas with 3 regions
make_toy_neurovol_atlas() # Plain NeuroVol atlas (like ASEG)
make_toy_setup()          # Atlas + mask + 4D data for reduce_atlas_vec tests
```

**Running tests:**
```r
devtools::test()                                    # Full suite
testthat::test_file("tests/testthat/test-sub-atlas.R")  # Single file (needs load_all first)
devtools::load_all(); testthat::test_file(...)      # Correct single-file pattern
```

### Common Pitfalls
- `testthat::test_file()` alone won't find package functions — always `devtools::load_all()` first, or use `devtools::test()`
- Warnings like "clustered volume only contains 1 partition" from neuroim2 are benign when subsetting to a single cluster
- TemplateFlow tests may fail without Python/scipy installed — always guard with `skip_on_cran()` or `tryCatch`

### Adding New Tests
1. Create `test-{feature}.R` in this directory
2. Add toy data helpers at the top — avoid requiring network downloads
3. Use `expect_*()` functions (never `stopifnot`)
4. Run `devtools::test()` to verify the full suite passes

## Dependencies

### Internal
- All tests depend on `R/` source code loaded via `devtools::load_all()`
- Toy helpers create `neuroim2` objects directly (NeuroSpace, NeuroVol, LogicalNeuroVol, ClusteredNeuroVol)

### External
- `testthat` (edition 3) — Test framework
- `neuroim2` — Data structures for synthetic test objects

<!-- MANUAL: -->
