# CIFTI adapter publication review

This review retains the [original qualification and failed attempts](../devbox-20261007/index.md)
and binds the final publication sources in [source-bindings.json](source-bindings.json).
The frozen exact-value acceptance contract remains unchanged.

## Review corrections

`replace_cifti_values()` now preserves previously unavailable samples. Supplying
a finite value or an unassigned label key cannot establish sampled support.
Regression tests exercise value replacement, file round trips and a second
cortical transform. In-memory value operations explicitly check optional XML
support. The expanded template API documents its `CiftiTransform` return type,
and all new/expanded reference pages render their cross-references as links.

## Final numerical evidence

- [Independent I/O oracle](io-oracle-receipt.json): all 17 NiBabel fixtures pass
  with exact values, matrix axes, brain models, metadata, label tables and extra
  extensions, including endian, singleton-map and NIfTI scaling cases.
- [Full-density cortical consumer](adapter-consumer-receipt.json): all eight
  scalar/label and axis-order cases pass in both fsaverage 164k/fsLR 32k
  directions. All 2,874,376 values match direct public surface application
  exactly; availability has zero mismatches. The receipt's measured source
  hashes match the final publication sources.
- Independent NiBabel checks of [downsampled](adapter-down-format-receipt.json)
  and [upsampled](adapter-up-format-receipt.json) outputs pass.
- [Workbench inspection](workbench-inspection-receipt.json) accepts every
  fixture for brain-model inspection. Transposed axes retain their documented
  unknown-subtype limitation.
- The [installed environment](environment.json) matches the original recorded
  package versions and compiled artifacts. Rebuild and bind the pinned engine
  on another machine rather than borrowing these binary identities.

This adapter preserves noncortical support and geometry in an explicitly
declared common MNI6/MNI2009c frame. Changed volume grids, frames or support are
rejected. Cortical domains require explicit caller bindings. Qualification
establishes adapter agreement, not Workbench resampling equivalence, anatomical
accuracy, area conservation or an inverse. Subcortical warping, backprojection
and additional surface densities remain future work.

## Engineering validation

[Engineering receipt](engineering-receipt.json):

- [Focused tests](focused-tests.log): 156 passes, zero failures/warnings/skips.
- [Full-suite summary](full-tests-summary.log): 2,926 passes, zero failures,
  44 warnings and two opt-in integration skips. The complete raw log is retained
  locally without alteration; its SHA-256 is recorded in the engineering receipt.
- [Package/manual check](package-check.log): zero errors, zero warnings and
  three notes for package size, unavailable time verification and missing HTML
  validation tooling. Tests run separately; this invocation uses `--no-tests`.
- [Project lint](project-lint.log) and [website build](website-build.log) pass.
- Subsequent documentation-only edits preserve every executable expression.
  [Documentation generation and Rd validation](documentation.log) and the
  [rendered reference checks](reference-links-all-pages.log) pass for all seven modified
  API pages.

An [initial focused invocation](initial-unbound-focused-tests.log) omitted the
engine-binding environment and skipped three cortical tests. The final focused
run loads that environment and executes all 156 assertions without skips.

Publication and hosted CI status are tracked by the feature PR; these receipts
describe local qualification and do not change the released 0.2.0 evidence.
