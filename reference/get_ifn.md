# Retrieve IFN regional layers by intersecting an area

Downloads (if necessary), caches and extracts IFN regional layers
intersecting a given area of interest.

## Usage

``` r
get_ifn(x, type = c("ser", "ser_ar", "rfn", "rfd", "zp"), cache = NULL)
```

## Arguments

- x:

  An `sf` object defining the input area of interest.

- type:

  A character string specifying the IFN regional dataset to use. Must be
  one of:

  - `"ser"`: sylvo-écorégions

  - `"ser_ar"`: sylvo-écorégions (aggregated regions)

  - `"rfn"`: réseau forestier national

  - `"rfd"`: réseau forestier départemental

  - `"zp"`: zones de production

- cache:

  A character string defining the cache directory. Defaults to a
  package-specific cache directory created with
  `tools::R_user_dir("Rsequoia2", which = "cache")`.

## Value

An `sf` object containing the region features intersecting `x`. Returns
`NULL` if no region intersects the input geometry.

## Details

The function retrieves official IGN regional datasets provided as
shapefiles in Lambert-93 projection. Downloaded archives are stored
locally and reused on subsequent calls.

The regional layer is automatically reprojected to match the coordinate
reference system of `x` before computing spatial intersections.

Only features intersecting the input geometry are returned. No geometry
modification (e.g. clipping) is applied.
