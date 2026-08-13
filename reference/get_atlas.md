# Load an Atlas by Registered ID

Dispatches to the loader registered for \`name\`, forwarding \`...\` to
it. This is a thin convenience wrapper: it lets callers request an atlas
by string id (e.g. \`"schaefer"\`, \`"glasser"\`, \`"aseg"\`) without
having to know which specific \`get\_\*\_atlas()\` function to call.
Aliases registered alongside the canonical id are matched
case/punctuation-insensitively.

## Usage

``` r
get_atlas(name, ...)
```

## Arguments

- name:

  Atlas id or alias; see \[list_atlases()\] for available choices.

- ...:

  Arguments forwarded to the registered loader function.

## Value

The loaded atlas object (class depends on the loader).

## See also

\[list_atlases()\].

## Examples

``` r
if (FALSE) { # \dontrun{
# Equivalent to get_aseg_atlas()
aseg <- get_atlas("aseg")

# Equivalent to get_schaefer_atlas(parcels = "100", networks = "7")
schaef <- get_atlas("schaefer", parcels = "100", networks = "7")
} # }
```
