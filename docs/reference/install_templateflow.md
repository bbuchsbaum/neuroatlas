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
#> TemplateFlow and scipy are already installed in: /Users/bbuchsbaum/Library/Preferences/org.R-project.R/R/neuroatlas/r-reticulate

# Force reinstallation
install_templateflow(force_reinstall = TRUE)
#> Installing packages using method: auto
#> Using virtual environment '/Users/bbuchsbaum/Library/Preferences/org.R-project.R/R/neuroatlas/r-reticulate' ...
#> + /Users/bbuchsbaum/Library/Preferences/org.R-project.R/R/neuroatlas/r-reticulate/bin/python -m pip install --upgrade --no-user scipy templateflow
#> Installation complete. Packages installed to: default Python environment
# }
```
