# Retrieve infrastructure polygon features around an area

Retrieve infrastructure polygon features around an area

## Usage

``` r
get_infra_poly(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object of type `POLYGON` containing infrastructure features with
standardized fields, including:

- `TYPE` - Infrastructure type code:

  - `BAT` = Building

  - `CIM` = Cemetery

  - `CST` = Surface construction

  - `AER` = Aerodrome runway

  - `SPO` = Sports ground

  - `VIL` = Urbanized area (importance 1-2)

  - `HAB` = Residential area

- `NAME` - Toponym when available

- `SOURCE` - Data source (`IGNF_BDTOPO_V3`)

## Details

The function retrieves several polygon infrastructure layers from the
IGN BDTOPO V3 dataset within a 1000 m convex buffer around `x`.

Retrieved layers include buildings, cemeteries, surface constructions,
aerodrome runways, sports grounds, and residential or urbanized areas.

If no infrastructure data are found, the function returns an empty
standardized `sf` object.
