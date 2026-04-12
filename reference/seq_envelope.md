# Compute convex hulls from buffered geometries

Generates convex hull polygons from an input `sf` object after applying
a buffer and union operation. The buffer step allows merging nearby or
disconnected geometries prior to computing the convex hull, making the
result more spatially coherent.

This function is useful for deriving generalized envelopes around
clusters of geometries while tolerating small gaps or spatial
fragmentation.

## Usage

``` r
seq_envelope(x, buffer = 0, crs = 2154)
```

## Arguments

- x:

  An `sf` object.

- buffer:

  `numeric`; Buffer distance (in CRS units) applied before union. This
  parameter controls the aggregation of nearby geometries. Larger values
  will merge more distant features. Default is `0`.

- crs:

  `numeric` or `character`; Target coordinate reference system used for
  geometric operations. Defaults to EPSG:2154 (Lambert-93).

## Value

An `sf` object containing convex hull polygons (one per connected
component).

## Details

The algorithm proceeds as follows:

1.  Validate geometries (`st_make_valid`)

2.  Reproject to the target CRS (`st_transform`)

3.  Apply a buffer of size `dist`

4.  Merge geometries (`st_union`)

5.  Split into individual polygon components (`st_cast`)

6.  Compute a convex hull for each component (`st_convex_hull`)

7.  Validate and return as an `sf` object

Note that the buffer step alters the geometry extent and may introduce
spatial generalization. The resulting convex hulls are therefore
dependent on the chosen `dist` value.

## See also

[`sf::st_convex_hull()`](https://r-spatial.github.io/sf/reference/geos_unary.html),
[`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html),
[`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html),
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
