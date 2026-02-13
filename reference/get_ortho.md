# Download orthophotos from the IGN WMS (RGB or IRC)

Downloads an orthophoto (RGB or infrared/IRC) from the IGN WMTS service
for the area covering `x` expanded with a buffer. The result is returned
as a masked `SpatRaster`, clipped to the input geometry to keep file
size minimal.

## Usage

``` r
get_ortho(
  x,
  type = c("irc", "rgb"),
  buffer = 200,
  zoom = 12,
  crs = 2154,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

- type:

  `character`; Type of orthophoto to download. Must be one of:

  - `"rgb"` - true-color orthophoto

  - `"irc"` - near-infrared orthophoto

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge the
  download area.

- zoom:

  `integer` between 0 and 21. The smaller the zoom level, the less
  precise the resolution(see
  [`happign::get_wmts()`](https://paul-carteron.github.io/happign/reference/get_wmts.html))

- crs:

  `numeric` or `character`; CRS of the returned raster (see
  [`happign::get_wmts()`](https://paul-carteron.github.io/happign/reference/get_wmts.html))

- overwrite:

  `logical`; If `TRUE`, file is overwritten.

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

`SpatRaster` object from `terra` package

## Details

The orthophoto retrieved contains data for the whole bounding box (bbox)
of `x` (plus the buffer). To reduce the final file size and avoid
unnecessary pixels, the raster is immediately masked with the buffered
geometry.

## See also

[`happign::get_wmts()`](https://paul-carteron.github.io/happign/reference/get_wmts.html)

## Examples

``` r
if (FALSE) { # \dontrun{

p <- sf::st_sfc(st_point(c(-4.372746579180652, 47.79820761331345)), crs = 4326)

ortho <- get_ortho(p, type = "rgb", buffer = 50)
irc <- get_ortho(p, type = "irc", buffer = 50)

terra::plotRGB(ortho)
terra::plotRGB(irc)

} # }
```
