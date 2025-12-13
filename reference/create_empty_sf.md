# Create an empty sf object

This function creates an empty `sf` object with the desired geometry
type (point, line, polygon, etc.).

## Usage

``` r
create_empty_sf(geom_type, ...)
```

## Arguments

- geom_type:

  `character`; one of the geometry types supported by `sf`: `POLYGON`,
  `LINESTRING`, `POINT`, `MULTIPOLYGON`, `MULTILINESTRING`,
  `MULTIPOINT`.

- ...:

  Named attributes with their classes. For example, you can pass
  arguments like `PLACETTE = character(0)`. Each argument must be named
  according to the attribute, with the corresponding empty vector of the
  appropriate class.

## Value

An `sf` object

## Examples

``` r
if (FALSE) { # \dontrun{
empty_sf <- create_empty_sf(
  "LINESTRING",
  PLACETTE = character(0),
  TSE_VOL = numeric(0)
)
} # }
```
