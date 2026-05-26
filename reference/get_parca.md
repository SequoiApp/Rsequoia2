# Download and format cadastral parcel(s)

Downloads parcel geometries from the Etalab cadastre API and optionally
replaces them with higher-quality BDP geometries when available.
Lieux-dits and administrative attributes (commune, departement, region)
are joined automatically.

## Usage

``` r
get_parca(idu, bdp_geom = FALSE, lieu_dit = FALSE, verbose = TRUE)
```

## Arguments

- idu:

  `character` Vector of IDU identifiers.

- bdp_geom:

  `logical` If `TRUE`, replace Etalab geometries with BDP when possible.

- lieu_dit:

  `logical` If `TRUE`, attach lieux-dits to each parcel.

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

An `sf` object of parcels with harmonized attributes.

## Details

The BDP ("Base de Donnees Parcellaire") is an older IGN product that is
no longer updated. It was originally derived from Etalab cadastral
parcels, but the geometries were manually corrected by IGN to better
match reality

Cadastral limits and the real terrain rarely match perfectly. Using BDP
may therefore improve spatial accuracy, but cannot guarantee exact
correspondence with legal cadastral boundaries.
