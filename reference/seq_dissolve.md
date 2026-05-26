# Dissolve polygons using buffer-based tolerance

Performs a topological dissolve (union) of polygon geometries using a
buffer-based approach to handle small gaps, overlaps, and sliver
geometries.

The method applies a positive buffer to expand geometries, merges them
using a union operation, and then applies a negative buffer to restore
the original extent. A small epsilon offset is used to avoid geometry
collapse during the shrinking phase.

This approach is particularly useful when dealing with imperfect polygon
coverages (e.g. digitizing errors, micro-gaps, or near-adjacent
features).

## Usage

``` r
seq_dissolve(
  x,
  buffer = 5,
  eps = 0.01,
  endCapStyle = "SQUARE",
  joinStyle = "MITRE",
  mitreLimit = 5,
  snapping = FALSE
)
```

## Arguments

- x:

  An `sf` object containing only `POLYGON` or `MULTIPOLYGON` geometries.

- buffer:

  `numeric`; Buffer distance (in coordinate units) used to expand
  geometries before union. This value controls the tolerance for merging
  nearby or slightly disconnected polygons. Default is `5`.

- eps:

  `numeric`; Small offset applied during the negative buffer step
  (`-buffer + eps`) to prevent topology collapse or invalid geometries.
  Should be a very small positive value relative to `buffer`. Default is
  `0.01`.

- endCapStyle:

  `character`; Shape of line endings used in buffering. One of
  `"ROUND"`, `"FLAT"`, or `"SQUARE"`. Passed to
  [`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- joinStyle:

  `character`; Shape of corners in buffering. One of `"ROUND"`,
  `"MITRE"`, or `"BEVEL"`. Passed to
  [`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- mitreLimit:

  `numeric`; Mitre ratio limit (only used if `joinStyle = "MITRE"`).
  Passed to
  [`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- snapping:

  `logical`; If `TRUE`, snaps the resulting geometries back to the
  original input geometries using
  [`sf::st_snap()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
  with the same tolerance as `buffer`. This can help reduce spatial
  drift introduced by buffering. Default is `FALSE`.

## Value

An `sf` object containing dissolved polygon geometries.

## Details

The algorithm proceeds as follows:

1.  Cast geometries to `POLYGON`

2.  Apply a positive buffer (`+buffer`)

3.  Perform a geometric union (`st_union`)

4.  Apply a negative buffer (`-buffer + eps`)

5.  Validate geometries (`st_make_valid`)

6.  Recast to `POLYGON`

Note that this method modifies geometry boundaries and may introduce
slight positional shifts. The magnitude of these shifts depends on the
chosen `buffer` and `eps` values.

## See also

[`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html),
[`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html),
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html),
[`sf::st_snap()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
