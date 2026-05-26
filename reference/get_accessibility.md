# Retrieve forest accessibility features (porter or skidder)

Builds a convex buffer around the input geometry and retrieves
accessibility features for the requested machine type.

## Usage

``` r
get_accessibility(
  x,
  type = c("porteur", "skidder"),
  buffer = 1000,
  verbose = TRUE
)
```

## Arguments

- x:

  An `sf` object defining the input area of interest.

- type:

  `character` Accessibility type. One of `"porter"` or `"skidder"`.

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge the
  download area.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

An `sf` object containing accessibility features, or `NULL` if none
found.
