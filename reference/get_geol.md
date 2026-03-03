# Download BRGM geology data to a geometry

Downloads BRGM geology layers for the departments intersecting `x` (or
for specified departments) and spatially filters them using a buffered
envelope around `x`.

## Usage

``` r
get_geol(
  x,
  key = "carhab",
  buffer = 100,
  deps = NULL,
  cache = NULL,
  verbose = FALSE,
  overwrite = FALSE
)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry used to determine relevant departments and to
  spatially filter geology data.

- key:

  `character` Source use to download geology from BRGM. Must be one of:

  - `"carhab"` : geological data used to created the `CarHab` dataset ;

  - `"bdcharm50"` : geological maps, vectorized and harmonised at
    1:50,000 scale.

- buffer:

  `numeric`; Buffer distance (in meters) applied to `x` before spatial
  filtering. Default is `100`.

- deps:

  `character` or `numeric`; One or sevral french department code (see
  [`get_cog()`](https://sequoiapp.github.io/Rsequoia2/reference/get_cog.md)).

- cache:

  `character`; Storage directory. Defaults to the user cache directory
  (see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, overwrite zipfile.

## Value

An `sf` object containing clipped geology features.

## Details

The function:

- Identifies departments intersecting `x` (if `deps` is not provided) ;

- Downloads geology layers using
  [`get_brgm()`](https://sequoiapp.github.io/Rsequoia2/reference/get_brgm.md)
  ;

- Filters features intersecting a buffered envelope around `x`.

All geometries are returned in EPSG:2154.
