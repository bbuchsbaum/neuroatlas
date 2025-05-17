# TemplateFlow Interface Refactoring Plan & Tickets

## Overall Goal
Create a more R-native, robust, and user-friendly interface to TemplateFlow, abstracting away `reticulate` details, enhancing caching, discoverability, and integration within `neuroatlas`.

## 1. Architecture & Core Wrapper

### 1.1. Central `templateflow` S3 Object
- [x] **T1.1.1**: Define S3 class `templateflow`.
- [x] **T1.1.2**: Implement constructor `create_templateflow()`:
    - [x] Initializes and stores the `reticulate` Python `templateflow.api` handle (e.g., `tf_api`).
    - [x] Establishes and stores a local cache path (e.g., using `tools::R_user_dir("neuroatlas", "cache", "templateflow")`).
    - [x] Stores user-configurable options (e.g., default template, verbosity level).
- [x] **T1.1.3**: Implement `print.templateflow` S3 method:
    - [x] Shows basic info: TemplateFlow library path, cache path, online status.
    - [x] Lists a few example available templates or summary of discoverability (see Section 5).
- [x] **T1.1.4**: Implement `$.templateflow` S3 method for dynamic, R-native access to TemplateFlow entities (e.g., `tf$MNI152NLin2009cAsym`). This should internally chain calls to the Python API.
- [x] **T1.1.5**: Implement `[[.templateflow` S3 method as an alternative or for programmatic access.
- [x] **T1.1.6**: (Optional) Implement `names.templateflow` to list available top-level attributes (e.g., template names accessible via `$`).

### 1.2. Auto-conversion & Utility Helpers
- [x] **T1.2.1**: Create internal helper `as_neurovol(py_object_or_path)`:
    - [x] Accepts Python Nifti object path strings, or `nibabel.Nifti1Image` objects.
    - [x] Converts to a `neuroim2::NeuroVol` object.
    - [x] Handles potential errors during conversion gracefully.
- [x] **T1.2.2**: Integrate `memoise` into `as_neurovol` if it involves re-reading from disk for the same path within a session, or directly into the file fetching part of `get_template`.

## 2. Retrieval API (High-Level Helper Functions)

### 2.1. Unified `get_template()` Function
- [x] **T2.1.1**: Design and implement the primary `get_template()` function with a signature like:
    `get_template(space = "MNI152NLin2009cAsym", variant = "brain", modality = "T1w", resolution = 1, cohort = NULL, desc = NULL, label = NULL, atlas = NULL, suffix = NULL, extension = ".nii.gz", path_only = FALSE, use_cache = TRUE, api_handle = NULL, ...)`
    - `space`: Template identifier (e.g., "MNI152NLin2009cAsym").
    - `variant`: High-level type (e.g., "brain", "head", "probseg", "mask", "dseg"). Replaces `desc` for common cases but allows `desc` for full specificity.
    - `modality`: Image type (e.g., "T1w", "T2w", "dwi"). Replaces `suffix` for common cases.
    - `resolution`: Numeric resolution in mm.
    - `cohort`, `desc`, `label`, `atlas`, `suffix`, `extension`: Pass through to TemplateFlow for fine-grained queries.
    - `path_only`: If `TRUE`, return file path string instead of `NeuroVol`.
    - `use_cache`: Boolean, to enable/disable caching for this call.
    - `api_handle`: Optionally pass an existing `templateflow` S3 object (from `create_templateflow()`) or let it use a default/global one.
    - `...`: Additional arguments to pass to `tflow$api$get()`.
- [x] **T2.1.2**: Internally, `get_template()` should construct the correct query dictionary for `tf_api$get()`.
- [x] **T2.1.3**: Implement robust validation for common parameter combinations:
    - E.g., if `variant = "probseg"`, `label` (for tissue type like GM/WM/CSF) should be specifiable.
    - E.g., `variant = "mask"` typically implies `modality = "mask"` (or `suffix="mask"`).
- [x] **T2.1.4**: Ensure `get_template()` returns a `NeuroVol` (via `as_neurovol`) or a file path string.

### 2.2. Typed Helper Functions (as thin wrappers)
- [x] **T2.2.1**: Refactor `get_template_brainmask(...)` to be a wrapper around `get_template(..., variant = "mask", modality = "mask")`.
- [x] **T2.2.2**: Refactor `get_template_probseg(..., label)` to be `get_template(..., variant = "probseg", label = label)`.
- [x] **T2.2.3**: Refactor `get_template_gm(...)` to be `get_template(..., variant = "probseg", label = "GM")`.
- [x] **T2.2.4**: Refactor `get_template_wm(...)` to be `get_template(..., variant = "probseg", label = "WM")`.
- [x] **T2.2.5**: Refactor `get_template_csf(...)` to be `get_template(..., variant = "probseg", label = "CSF")`.
- [x] **T2.2.6**: Refactor `get_template_head(...)` to be `get_template(..., variant = "head", modality = "T1w")`.
- [x] **T2.2.7**: Refactor `get_template_schaefer(...)` (the TemplateFlow one) to use the new `get_template()` with appropriate `atlas`, `desc`, `suffix` args.
- [x] **T2.2.8**: Review if original `get_template()` needs to be kept or if its functionality is fully superseded by the new `get_template()`. If kept for backward compatibility, mark as deprecated.

### 2.3. Vectorized Retrieval (Optional Enhancement)
- [x] **T2.3.1**: (Optional) Consider allowing vectorized arguments for `space`, `resolution`, `variant` in `get_template()`, returning a named list of `NeuroVol` objects or paths.

### 2.4 Typed Helper Functions for Volumes and Surfaces
- [ ] **T2.4.1**: Implement `get_volume_template(template, type, ...)` wrapper. (Superseded by `get_template`)
- [x] **T2.4.2**: Implement `get_surface_template(template_id, surface_type, hemi, ...)` wrapper.

## 3. Caching & Memoisation

### 3.1. Cache Directory Management
- [x] **T3.1.1**: Implement an internal function `neuroatlas_cache_dir(subdir = NULL)` that uses `tools::R_user_dir("neuroatlas", "cache")` and creates `subdir` if it doesn't exist. `templateflow` cache will be in `neuroatlas_cache_dir("templateflow")`.
- [x] **T3.1.2**: Ensure `tflow$api$get()` downloads are directed to this cache directory. TemplateFlow's Python library itself has caching; ensure R wrapper leverages it and R session-level memoisation is complementary.

### 3.2. Session-Level Memoisation
- [x] **T3.2.1**: Apply `memoise::memoise()` to the core TemplateFlow fetching operation within `get_template()` (or the part that calls `tf_api$get()`).
    - The memoisation key should be a unique, reproducible string derived from all query parameters (name, resolution, desc, label, etc.).
- [x] **T3.2.2**: Ensure memoisation stores the *file path* of the downloaded template. Subsequent calls for the same template (that would read the file) can then use the memoised path.

### 3.3. Cache Control Functions
- [x] **T3.3.1**: Implement `clear_templateflow_cache()` to remove all files from the `neuroatlas_cache_dir("templateflow")`.
- [x] **T3.3.2**: (Optional) Add `show_templateflow_cache_path()` function.

## 4. Error Handling & Validation

### 4.1. Pre-emptive Checks and Graceful Errors
- [x] **T4.1.1**: In `get_template()`, before calling `tf_api$get()`, use `tf_api$template_exists(space)` or similar to check if the base template exists.
- [x] **T4.1.2**: Validate `resolution` against available resolutions for a template (e.g., using `tf_api$resolutions(space)` if such a method exists, or by attempting a fetch and catching errors).
- [x] **T4.1.3**: Wrap Python calls (`tf_api$get()`, etc.) in `tryCatch` or `reticulate::py_capture_error` to convert Python exceptions into R conditions (errors or warnings).
- [x] **T4.1.4**: Provide user-friendly R error messages (e.g., "Template 'XYZ' not found." or "Resolution 'R'mm not available for template 'XYZ'.") instead of raw Python tracebacks.

### 4.2. Offline Mode & Cache Fallback
- [ ] **T4.2.1**: Detect network unavailability (e.g., if `tf_api$get()` fails due to network issues).
    - *Note: Partially handled. Python errors are caught; specific network error detection is complex. Current behavior is to error on fetch failure.*
- [x] **T4.2.2**: If offline and `use_cache = TRUE`, `get_template()` should attempt to find the requested template in the local cache first.
    - *Note: Handled by `memoise` (R-level path cache) and TemplateFlow Python's own disk cache (`TEMPLATEFLOW_HOME`). If a path/file was previously fetched successfully, it will be served from these caches without a new network request.*
- [ ] **T4.2.3**: If offline and not found in cache, issue a warning and return `NULL` or error gracefully.
    - *Note: Currently errors out if a new item cannot be fetched (e.g. due to network issues). Graceful NULL return would require more specific network error detection and possibly a new parameter to `get_template`.* 

## 5. Discoverability Helper Functions

- [x] **T5.1.1**: Implement `tflow_spaces(pattern = NULL)` wrapping `tf_api$templates()`.
- [x] **T5.1.2**: Implement `tflow_files(space, query_args = NULL)`:
    - A more general way to query available files/metadata for a given template `space`.
    - `query_args` could be a list of other TemplateFlow parameters (desc, suffix, etc.) to filter results.
- [x] **T5.1.3**: Revisit `print.templateflow` (T1.1.3) to make use of these listing functions for a more informative summary.

## 6. Inter-package Integration

### 6.1. Universal Template Input in `neuroatlas`
- [x] **T6.1.1**: Identify all `neuroatlas` functions that currently accept `NeuroVol` objects as templates (e.g., for resampling targets, defining spaces).
- [x] **T6.1.2**: For each identified function, modify its signature or add a new argument to also accept:
    - A TemplateFlow query string (e.g., `"MNI152NLin2009cAsym:1mm:brain"`).
    - A list of TemplateFlow query parameters (e.g., `list(space="MNI", resolution=1, variant="brain")`).
- [x] **T6.1.3**: Implement an internal helper function `resolve_template_input(input)` that:
    - Checks if `input` is already a `NeuroVol`.
    - If `input` is a string or list, parses it and calls the new `get_template()` to fetch the `NeuroVol`.
- [x] **T6.1.4**: Update `get_schaefer_atlas(..., outspace)` so `outspace` can be a TemplateFlow query string/list, resolved via `resolve_template_input()`.
- [x] **T6.1.5**: Update `resample(vol, outspace, ...)` so `outspace` can be a TemplateFlow query string/list (primarily achieved by `get_schaefer_atlas` passing a resolved space).
- [x] **T6.1.6**: Update `dilate_atlas(atlas, mask, ...)` so `mask` can be a TemplateFlow query string/list, resolved via `.resolve_template_input()` to fetch a brain mask.

## 7. Documentation & Vignettes

- [x] **T7.1.1**: Write comprehensive `roxygen2` documentation for the new unified `get_template()` function, explaining all parameters and common use cases.
- [x] **T7.1.2**: Document all other new/refactored public functions (`create_templateflow`, `tflow_spaces`, `tflow_files`, `clear_templateflow_cache`, etc.).
- [x] **T7.2.1**: Create a new vignette: `vignettes/working-with-templateflow.Rmd`.
- [x] **T7.2.2**: Vignette content:
    - Introduction to TemplateFlow concept.
    - Setting up (e.g., `create_templateflow()`).
    - Discovering available templates/metadata (`tflow_spaces`, `tflow_files`).
    - Retrieving various template types (T1w, masks, probsegs) using `get_template()` and typed helpers.
    - Understanding caching behavior and management.
    - Examples of integrating TemplateFlow templates with other `neuroatlas` functions (e.g., `reduce_atlas`, plotting functions if applicable).

## 8. Backward-Compatibility Layer

- [ ] **T8.1.1**: For all existing `get_template_*` functions in `R/template_flow.R` that are being replaced by the new `get_template()` or its wrappers:
    - Mark them with `@deprecate new="get_template"` (or the relevant new helper).
    - Add `@keywords internal` if they are no longer intended for direct user invocation once deprecated.
- [ ] **T8.1.2**: Modify the body of these old functions to call the new unified `get_template()` or relevant new helper, ensuring parameters are correctly mapped.
- [ ] **T8.1.3**: Ensure they issue a deprecation warning when called (e.g., using `lifecycle::deprecate_warn()`).

## 9. Implementation Milestones (Phased Rollout)

### M1: Core Wrapper & Foundational Retrieval
*   Target Tickets: T1.1.1, T1.1.2, T1.1.3, T1.2.1, T2.1.1 (basic implementation), T2.1.2, T2.1.4, T3.1.1, T4.1.3.
*   Goal: Establish the `templateflow` S3 object and a working, albeit basic, `get_template()` that can fetch a T1w image.

### M2: Full Retrieval API & Typed Helpers
*   Target Tickets: T2.1.1 (full signature), T2.1.3, T2.2.1-T2.2.8, T8.1.1-T8.1.3 (for functions being wrapped).
*   Goal: Complete the `get_template()` API and refactor all existing typed helpers. Implement deprecation strategy.

### M3: Caching, Memoisation & Discoverability
*   Target Tickets: T1.2.2, T3.1.2, T3.2.1, T3.2.2, T3.3.1, T3.3.2, T5.1.1, T5.1.2, T5.1.3, T1.1.4-T1.1.6.
*   Goal: Implement robust caching and session-level memoisation. Add functions for users to explore TemplateFlow resources. Flesh out `templateflow` S3 object methods.

### M4: Error Handling & Validation
*   Target Tickets: T4.1.1, T4.1.2, T4.1.4, T4.2.1, T4.2.2, T4.2.3.
*   Goal: Make the interface resilient to errors, offline scenarios, and invalid user input.

### M5: Inter-package Integration
*   Target Tickets: T6.1.1, T6.1.2, T6.1.3, T6.1.4, T6.1.5.
*   Goal: Enable seamless use of TemplateFlow resources within other `neuroatlas` functions.

### M6: Documentation, Vignette & Final Deprecation
*   Target Tickets: T7.1.1, T7.1.2, T7.2.1, T7.2.2. Review all deprecations.
*   Goal: Provide comprehensive user guides and ensure backward compatibility story is clear.

### M7: Final Review, Testing & Cleanup
*   Target Tickets: Comprehensive unit tests for all new/refactored functions. Review and remove any helper code from development (e.g. `.create_schaefer_alias` if not moved to `data-raw`). Code style checks. CRAN pre-checks.
*   Goal: Prepare for a stable release.

This detailed ticket list should provide a clear roadmap for the refactoring effort.
