# Create management unit field (UG) in the *UA* sf object

Generates a standardized management unit identifier (UG) in the *UA*
object based on configured parcel keys.

## Usage

``` r
ua_generate_ug(
  ua,
  ug_keys = c("parcelle", "sous_parcelle"),
  separator = ".",
  verbose = TRUE
)
```

## Arguments

- ua:

  `sf` object containing analysis units; must contain fields used by
  `ug_keys`.

- ug_keys:

  `character` vector, default `c("parcelle", "sous_parcelle")`. Keys
  used to build the UG identifier.

- separator:

  `character`, default `"."`. Separator between keys.

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

`sf` object UA with UG field filled.
