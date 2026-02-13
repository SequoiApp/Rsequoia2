# Retrieve point infrastructure features around an area

Retrieve point infrastructure features around an area

## Usage

``` r
get_infra_point(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object of type `POINT` containing point infrastructure features
with standardized fields, including:

- `TYPE` - Infrastructure type code derived from BDTOPO nature values:

  - `PYL` = Pylon / antenna

  - `CLO` = Steeple

  - `CRX` = Cross or calvary

  - `EOL` = Wind turbine

  - `CST` = Other point construction

  - `GRO` = Cave

  - `GOU` = Sinkhole

  - `ORO` = Other orographic detail

- `NAME` - Toponym when available

- `SOURCE` - Data source (`IGNF_BDTOPO_V3`)

## Details

The function retrieves point infrastructure layers from the IGN BDTOPO
V3 dataset within a 1000 m convex buffer around `x`.

Retrieved layers include point constructions, orographic details, and
pylons. Feature types are classified into standardized Sequoia codes
based on their original `nature` attribute.

If no point infrastructure data are found, the function returns an empty
standardized `sf` object.
