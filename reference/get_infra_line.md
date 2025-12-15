# Retrieve linear infrastructure features around an area

Retrieve linear infrastructure features around an area

## Usage

``` r
get_infra_line(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object of type `LINESTRING` containing linear infrastructure
features with standardized fields, including:

- `TYPE` — Infrastructure type code:

  - `CST` = Linear construction

  - `LEL` = Power line

  - `ORO` = Orographic line

  - `VFE` = Railway line

- `NAME` — Toponym when available

- `NATURE` — Additional attribute (e.g. voltage for power lines)

- `SOURCE` — Data source (`IGNF_BDTOPO_V3`)

## Details

The function retrieves linear infrastructure layers from the IGN BDTOPO
V3 dataset within a 1000 m convex buffer around `x`.

Retrieved layers include linear constructions, power lines, orographic
lines, and railway segments.

If no linear infrastructure data are found, the function returns an
empty standardized `sf` object.
