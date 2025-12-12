# Download and read BRGM "BD Charm 50" harmonised geology layers

Downloads and reads the geology layer (`S_FGEOL_*.shp`) from BRGM "BD
Charm 50" ZIP archives for one or several French departments.

## Usage

``` r
get_brgm(
  deps,
  source = "carhab",
  cache = NULL,
  verbose = FALSE,
  overwrite = FALSE
)
```

## Arguments

- deps:

  `character` or `numeric`; One or sevral french department code (see
  [happign::dep_2025](https://paul-carteron.github.io/happign/reference/dep_2025.html)).

- source:

  `character` Source use to download geology from BRGM. Must be one of:

  - `"carhab"` : geological data used to created the `CarHab` dataset ;

  - `"bdcharm50"` : geological maps, vectorized and harmonised at
    1:50,000 scale.

- cache:

  `character`; Storage directory. Defaults to the user cache directory
  (see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, overwrite zipfile.

## Value

An `sf` object containing the geology features for all requested
departments.

`sf`

## Details

Multiple departments are combined into a single `sf` object.

For each department:

- The corresponding BRGM ZIP archive is obtained via
  [`download_brgm()`](https://mucau.github.io/Rsequoia2/reference/download_brgm.md)
  ;

- The geology shapefile (`S_FGEOL_*.shp`) is imported ;

- The layer is read directly from the ZIP using
  `sf::read_sf("/vsizip/...")` without extracting files ;

The resulting layers are row-bound into a single `sf` object. All
geometries are returned in the CRS provided by BRGM (typically
EPSG:2154).
