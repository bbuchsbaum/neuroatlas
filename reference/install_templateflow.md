# Install Templateflow Python package

Installs the Python packages \`scipy\` and \`templateflow\` into a
persistent virtual environment. This ensures the packages remain
available across R sessions.

## Usage

``` r
install_templateflow(
  method = "auto",
  conda = "auto",
  envname = NULL,
  force_reinstall = FALSE
)
```

## Arguments

- method:

  Installation method. Options are "auto" (default), "virtualenv", or
  "conda". Using "virtualenv" is recommended for persistent
  installations.

- conda:

  Path to a conda executable or `"auto"` to let `reticulate` locate one.
  Only used if method = "conda".

- envname:

  Name of the virtual environment. Defaults to "r-neuroatlas" for
  persistence.

- force_reinstall:

  Logical. If TRUE, forces reinstallation even if packages exist.

## Value

Invisible `NULL` indicating success.

## Details

This function creates a persistent Python environment specifically for
neuroatlas, preventing the need to reinstall TemplateFlow in each R
session. The environment is stored in a user-specific directory that
persists across sessions.

## Examples

``` r
# \donttest{
# Install with default settings (recommended)
install_templateflow()
#> Installing packages using method: auto
#> Warning: An ephemeral virtual environment managed by 'reticulate' is currently in use.
#> To add more packages to your current session, call `py_require()` instead
#> of `py_install()`. Running:
#>   `py_require(c("scipy", "templateflow"))`
#> Installation complete. Packages installed to: default Python environment

# Force reinstallation
install_templateflow(force_reinstall = TRUE)
#> Installing packages using method: auto
#> Warning: An ephemeral virtual environment managed by 'reticulate' is currently in use.
#> To add more packages to your current session, call `py_require()` instead
#> of `py_install()`. Running:
#>   `py_require(c("scipy", "templateflow"))`
#> Installation complete. Packages installed to: default Python environment
# }
```
