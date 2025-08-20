# CRAN Submission Notes for neuroatlas

## Current Status

The package has been improved to be CRAN-ready with the following fixes:

### Fixed Issues:
1. ✅ Removed all non-ASCII characters
2. ✅ Replaced all `library()` calls with `::` notation
3. ✅ Added missing imports to NAMESPACE
4. ✅ Documented all exported datasets
5. ✅ Fixed data() calls to use proper environments
6. ✅ Added missing parameter documentation
7. ✅ Created proper .Rbuildignore file
8. ✅ Set LazyDataCompression in DESCRIPTION
9. ✅ Fixed all NSE issues with .data pronoun
10. ✅ Fixed example errors
11. ✅ Added methods to Imports

### Remaining Issues:

#### 1. WARNING: Missing or unexported objects
The package uses three internal functions from dependencies:
- `neuroim2:::extract_roi_data`
- `neuroim2:::is.NeuroSpace`
- `reticulate::py_capture_error` (replaced with tryCatch)

**Action Required**: Contact the neuroim2 package maintainer to export these functions, or refactor code to use only exported functions.

#### 2. NOTE: Many package dependencies
The package imports 24 packages. While this is acceptable for complex packages, consider:
- Moving optional functionality to Suggests
- Using conditional loading for features that not all users need

#### 3. NOTE: Large installed size (23.6Mb)
This is primarily due to the included atlas data (23.1Mb). This is acceptable for a neuroimaging atlas package, but consider:
- Using LazyDataCompression (already done)
- Potentially hosting larger datasets externally

## Pre-submission Checklist:

- [ ] Resolve neuroim2 internal function usage
- [ ] Run `R CMD check --as-cran` on multiple platforms
- [ ] Test with R-devel
- [ ] Update NEWS.md with changes
- [ ] Ensure all examples run in < 5 seconds
- [ ] Check reverse dependencies (if any)
- [ ] Write cran-comments.md for submission

## Submission Notes:

This is a new submission. The large data size is necessary as this package provides neuroimaging atlases that require high-resolution spatial data.