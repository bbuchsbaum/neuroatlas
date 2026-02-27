# Spin Test for Spatial Correlation Significance

Tests whether the spatial correlation between two parcellated brain maps
is stronger than expected by chance, using the spin-test framework of
Alexander-Bloch et al. (2018). Parcel centroids on a sphere surface are
randomly rotated and reassigned to the nearest original centroid,
generating a null distribution of correlations.

## Usage

``` r
spin_test(
  map1,
  map2,
  atlas,
  n_perm = 1000L,
  cor_method = c("pearson", "spearman", "kendall"),
  sphere = NULL,
  seed = NULL
)
```

## Arguments

- map1:

  Numeric vector of parcel values (length K, aligned to atlas parcels).

- map2:

  Numeric vector of parcel values (length K, aligned to atlas parcels).

- atlas:

  A surface atlas object (class `"surfatlas"`) with `lh_atlas` and
  `rh_atlas` fields, or any atlas whose geometry can provide sphere
  coordinates.

- n_perm:

  Integer. Number of spin permutations. Default: 1000.

- cor_method:

  Character. Correlation method passed to
  [`cor`](https://rdrr.io/r/stats/cor.html). One of `"pearson"`,
  `"spearman"`, or `"kendall"`. Default: `"pearson"`.

- sphere:

  Optional list with elements `lh` and `rh`, each an N x 3 matrix of
  sphere vertex coordinates. If `NULL` (default), sphere coordinates are
  fetched via
  [`load_surface_template`](load_surface_template.md)`(..., "sphere")`.

- seed:

  Optional integer seed for reproducibility.

## Value

A list of class `"spin_test"` with:

- observed:

  The observed correlation between `map1` and `map2`.

- null_distribution:

  Numeric vector of length `n_perm` containing null correlations.

- p_value:

  Two-sided p-value: proportion of null correlations with absolute value
  \>= the observed absolute correlation.

- n_perm:

  Number of permutations performed.

## Details

The algorithm:

1.  Compute parcel centroids on the sphere surface by averaging the
    sphere coordinates of vertices belonging to each parcel.

2.  For each permutation, generate a uniform random 3D rotation matrix
    (from the Haar measure on SO(3)), rotate all centroids, then
    reassign each rotated centroid to its nearest original centroid
    using [`nn`](https://rdrr.io/pkg/Rnanoflann/man/nn.html).

3.  Compute the correlation of `map1` with the permuted `map2` (values
    shuffled according to the reassignment).

4.  The p-value is
    `(sum(abs(null) >= abs(observed)) + 1) / (n_perm + 1)`.

## References

Alexander-Bloch, A. F., et al. (2018). On testing for spatial
correspondence between maps of human brain structure and function.
*NeuroImage*, 178, 540-551.

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_schaefer_surfatlas(parcels = "100", networks = "7")
K <- length(atlas$ids)
map1 <- rnorm(K)
map2 <- map1 + rnorm(K, sd = 0.5)
res  <- spin_test(map1, map2, atlas, n_perm = 500, seed = 42)
print(res)
} # }
```
