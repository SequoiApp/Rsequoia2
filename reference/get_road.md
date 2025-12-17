# Retrieve road sections around an area

Retrieve road sections around an area

## Usage

``` r
get_road(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object of type `LINESTRING` containing road sections with
standardized fields, including:

- `TYPE` — Road section type code, deduced from surface, numbering and
  importance attributes:

  - `RN` = National road (autoroutes, European roads, numbered roads
    starting with `A`, `E`, `N`, or departmental roads numbered \> 900,
    as well as major slip roads)

  - `RD` = Departmental road (numbered roads starting with `D`)

  - `RC` = Communal road (other paved roads)

  - `RF` = Forest or gravel road (unpaved / empierrée)

  - `PN` = Natural path (tracks, trails, footpaths)

- `NATURE` — Original BDTOPO road nature (surface / usage description)

- `NAME` — Road identifier, taken from `cpx_numero` when available,
  otherwise from `cpx_toponyme_route_nommee`

- `SOURCE` — Data source (`BDTOPO V3`)

## Details

The function retrieves road section layer from the IGN BDTOPO V3 dataset
within a 1000 m convex buffer around `x`.

If no road section data are found, the function returns an empty
standardized `sf` object.
