# Create management unit field (UG) in the *UA* sf object

Generates a standardized management unit identifier (UG) in the *UA*
object based on configured parcel keys.

## Usage

``` r
ua_generate_ug(
  ua,
  ug_keys = c("pcl_code", "sub_code"),
  separator = ".",
  verbose = TRUE
)
```

## Arguments

- ua:

  `sf` object containing analysis units; must contain fields used by
  `ug_keys`.

- ug_keys:

  `character` vector, default `c("pcl_code", "sub_code")`. Keys used to
  build the UG identifier.

- separator:

  `character`, default `"."`. Separator between keys.

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

`sf` object UA with UG field filled.
