# Repository Guidelines

## Project Structure & Module Organization
- Core package code (atlas loaders, helpers, plotting) lives in `R/`.
- Tests are under `tests/testthat/` in files named `test-*.R` that mirror topics in `R/`.
- Function documentation is generated from roxygen comments in `R/` and stored in `man/`.
- Long-form docs and tutorials live in `vignettes/`; packaged data live in `data/`, with raw sources and scripts in `data-raw/`.
- The pkgdown site is configured via `_pkgdown.yml` and built into `docs/` using content from `pkgdown/`.

## Build, Test, and Development Commands
- Install development dependencies: run `devtools::install_dev_deps()` in an R session.
- Load the package for interactive work: `devtools::load_all()`.
- Run the test suite: `devtools::test()` or `testthat::test_local("tests/testthat")`.
- Full package check (mirrors CI): `devtools::check()`; run this before significant PRs.
- Optional checklist/CRAN-style review: `checklist::check_pkg()` when the `checklist` package is available.
- Build the documentation website locally: `pkgdown::build_site()`.

## Coding Style & Naming Conventions
- Follow tidyverse-style conventions: 2-space indentation, no tabs, and a soft 80-character line width.
- Use `snake_case` for functions and objects; use `UpperCamelCase` for S3/S4 classes and key data types.
- Keep one main topic per file (e.g., `atlas.R`, `schaefer.R`); prefer descriptive, lowercase file names.
- Document all exported functions and classes with roxygen2 (`#'`) and regenerate docs with `devtools::document()`.

## Testing Guidelines
- Tests use `testthat` (edition 3) in `tests/testthat/` (see e.g. `test-dilation.R`, `test-space-transforms.R`).
- Name tests after the feature or behaviour they cover and keep them small and focused.
- Use `skip_on_cran()` or similar skips for network-dependent, slow, or TemplateFlow-related tests.
- CI tracks coverage via Codecov; avoid reducing overall coverage and add tests with every feature or bug fix.

## Commit & Pull Request Guidelines
- Write concise, present-tense commit messages (e.g., “Add Schaefer atlas loader”, “Fix template flow paths”).
- Group related changes together and avoid mixing refactors with behavioural changes in the same commit.
- Before opening a PR, ensure `devtools::check()` and `devtools::test()` pass; run `lintr::lint_package()` if available.
- PR descriptions should state motivation, key changes, how they were tested, and link issues (e.g., `Fixes #12`); add screenshots or rendered links for documentation and plotting changes when useful.

## Agent-Specific Notes
- Automated tools and LLM-based agents should make minimal, well-scoped changes and keep them consistent with existing patterns.
- Avoid introducing new dependencies, configuration files, or architectural styles without prior discussion in an issue or PR.
