# Retrieve road sections around an area

Retrieve road sections around an area

## Usage

``` r
get_road(x, buffer = 1000)
```

## Arguments

- x:

  An `sf` object used as the input area.

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge

## Value

An `sf` object of type `LINESTRING` containing road sections with
standardized fields, including:

- `TYPE` - Road section type code, deduced from surface, numbering and
  importance attributes:

  - `RN` = National road (autoroutes, European roads, numbered roads
    starting with `A`, `E`, `N`, or departmental roads numbered \> 900,
    as well as major slip roads)

  - `RD` = Departmental road (numbered roads starting with `D`)

  - `RC` = Communal road (other paved roads)

  - `RF` = Forest or gravel road (unpaved / empierree)

  - `PN` = Natural path (tracks, trails, footpaths)

  - `LY` = Tie ridge

- `NATURE` - Original BDTOPO road nature (surface / usage description)

- `IMPORTANCE` - Road importance, taken from `importance` when
  available,

- `PRIVE` - Road status, taken from `prive` when available,

- `RESTRICTION` - Road restriction, taken from
  `restriction_de_poids_total` when available,

- `NOM` - Road identifier, taken from `cpx_numero` when available,
  otherwise from `cpx_toponyme_route_nommee`

- `SOURCE` - Data source (`BDTOPO V3`)

## Details

The function retrieves road section layer from the IGN BDTOPO V3 dataset
within a convex buffer around `x`.

If no road section data are found, the function returns an empty
standardized `sf` object.
