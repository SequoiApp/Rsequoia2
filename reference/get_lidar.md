# Download and extract LiDAR HD raster

Downloads IGN LiDAR HD tiles for an area of interest, builds a VRT,
masks the raster to a buffered envelope around \`x\`, and returns it in
\`crs\`.

## Usage

``` r
get_lidar(
  x,
  key = c("mnt", "mns", "mnh"),
  buffer = 200,
  crs = 2154,
  cache = NULL,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  \`sf\` or \`sfc\`; Area used to select intersecting LIDAR tiles.

- key:

  \`character\`; LIDAR product to download. One of \`"mnt"\`, \`"mns"\`
  or \`"mnh"\`.

- buffer:

  \`numeric\`; Buffer distance, in meters, applied around \`x\` before
  masking.

- crs:

  \`integer\` or \`character\`; Target CRS of the returned raster.
  Defaults to EPSG:2154.

- cache:

  \`character\`; Cache directory. If \`NULL\`, the appropriate Rsequoia2
  LIDAR cache is used, see \[Rsequoia2::seq_cache()\].

- overwrite:

  \`logical\`; If \`TRUE\`, re-download existing tiles.

- verbose:

  \`logical\`; If \`TRUE\`, display messages.

## Value

Invisibly returns a \`terra::SpatRaster\`.
