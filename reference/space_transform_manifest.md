# Space Transform Manifest

Returns known transforms between coordinate/template spaces from a
static registry shipped with the package.

## Usage

``` r
space_transform_manifest(status = NULL)
```

## Arguments

- status:

  Optional character vector to filter by status (e.g., \`"available"\`,
  \`"planned"\`).

## Value

A data frame with one row per transform route and columns:
\`from_space\`, \`to_space\`, \`transform_type\`, \`backend\`,
\`confidence\`, \`reversible\`, \`data_files\`, \`status\`, and
\`notes\`.

## Examples

``` r
# All known routes
reg <- space_transform_manifest()

# Only implemented routes
available <- space_transform_manifest(status = "available")
```
