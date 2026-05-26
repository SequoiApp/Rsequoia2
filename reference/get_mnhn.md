# Download MNHN vector layer from IGN WFS (RGB or IRC)

Downloads a vector layer from the IGN WFS service for the area covering
`x` expanded with a buffer. Available layer are from MNHN (Nationale
Museum of Natural History)

## Usage

``` r
get_mnhn(x, key, buffer = 500, verbose = TRUE)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

- key:

  `character`; Layer to download. Must be one of from `get_keys("mnhn")`

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge the
  download area.

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

`sf` object from `sf` package

## See also

[`happign::get_wfs()`](https://paul-carteron.github.io/happign/reference/get_wfs.html)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

x <- st_sfc(st_point(c(-4.372746579180652, 47.79820761331345)), crs = 4326)

ospar <- get_mnhn(x, key = "ospar", buffer = 5000)
x <- st_transform(x, st_crs(ospar))

plot(st_geometry(ospar), col = "lightgrey")
plot(st_geometry(st_buffer(x, 5000)), col = "#FFB3B3", add = TRUE)
plot(st_geometry(x), col = "red", lwd =2, add = TRUE, pch = 19, cex = 2)

} # }
```
