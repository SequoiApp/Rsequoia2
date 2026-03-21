# Retrieve infrastructure polygon features around an area

Retrieve infrastructure polygon features around an area

## Usage

``` r
get_infra_poly(x, buffer = 1000)
```

## Arguments

- x:

  An `sf` object used as the input area.

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge

## Value

An `sf` object of type `POLYGON` containing infrastructure features with
standardized fields, including:

- `TYPE` - Infrastructure type code:

  - `AER` = Aerodrome runway

  - `BAT` = Building

  - `CIM` = Cemetery

  - `CST` = Surface construction

  - `HAB` = Residential area

  - `SPO` = Sports ground

  - `VIL` = Urbanized area (importance 1-2)

- `NAME` - Toponym when available

- `SOURCE` - Data source (`IGNF_BDTOPO_V3`)

## Details

The function retrieves several polygon infrastructure layers from the
IGN BDTOPO V3 dataset within a convex buffer around `x`.

Retrieved layers include buildings, cemeteries, surface constructions,
aerodrome runways, sports grounds, and residential or urbanized areas.

If no infrastructure data are found, the function returns an empty
standardized `sf` object.
