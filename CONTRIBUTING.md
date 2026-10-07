# Contributing to neuroatlas

Report bugs or propose changes through [GitHub
issues](https://github.com/bbuchsbaum/neuroatlas/issues). Include a
small reproducible example,
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html), and the
atlas/template identifiers. Describe the intended behavior and any
download or optional-package requirements.

Work on a branch and submit a pull request against the repository’s
default branch. Read `AGENTS.md` and the instructions for the
directories you change. Use two-space indentation, snake_case function
names, and synthetic atlas helpers in unit tests. Keep changes scoped
and document every exported function with roxygen2, including return
values and a short example. Generate documentation with
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html);
do not edit `NAMESPACE` or `man/` by hand.

The repository’s `.lintr` profile checks maintained R code for
formatting and correctness. Function names use snake_case, with
UpperCamelCase constructors and the existing MNI conversion functions
accepted. Established public argument names and mathematical local
variables are preserved. The 80-column target excludes string contents
so URLs and diagnostic text keep their values. Both supported pipe forms
and explicit or implicit returns are accepted. Short single-line guards
follow existing package conventions. There is no project rule limiting
identifier length or cyclomatic complexity; review complex changes with
their tests and numerical evidence. Downloaded libraries, generated
websites and frozen qualification snapshots are excluded so their bytes
and historical results remain intact. New maintained source is checked
under the same profile as existing code.

The project CI lint gate covers package, test and vignette code. Data
builders and scientific qualification protocols have their own
dependency environments and execution checks. Run
`Rscript .github/scripts/lint.R` to reproduce the CI gate. The OS
matrix, generated documentation, package check, website and coverage
measurement are required. rOpenSci diagnostics remain visible for
review; branch naming and complexity recommendations do not determine
the project gate.

Install development dependencies from `DESCRIPTION`. The reproducible
surface engine setup and machine-specific build receipts are described
in
[`plans/resume-neuroatlas.md`](https://bbuchsbaum.github.io/neuroatlas/plans/resume-neuroatlas.md).
New runtime dependencies or architectural changes should be discussed in
an issue first.

Run these checks before submitting:

``` r

devtools::document()
devtools::test(stop_on_failure = TRUE)
devtools::check(manual = TRUE)
```

Load the package with
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
before running an individual
[`testthat::test_file()`](https://testthat.r-lib.org/reference/test_file.html).
Network and slow integration tests must use `skip_on_cran()` and opt-in
fixtures where appropriate. Report warnings, skips and unavailable
checks explicitly. Do not count a skipped check as a pass.

Template mappings need exact input identities, immutable checksums,
declared direction and data semantics, and independent numerical
qualification. Freeze acceptance thresholds before evaluating
candidates. Preserve failed evidence; do not widen thresholds to make an
existing candidate pass. Interpolation agreement does not establish
anatomical accuracy, an inverse, or conservation. Follow the licenses of
externally downloaded inputs separately from this package’s code
license.

Mote’s Git-tracked `.mote/ops/` history is the shared work board. Set a
distinct actor, inspect `mote board`, and claim/reserve the bounded task
before editing. Do not run `mote init` in this repository. Commit and
push relevant Mote operations with the code, and release
claims/reservations when handing work off.
