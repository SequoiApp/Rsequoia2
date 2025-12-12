# Check management unit (UG) consistency in the *UA* sf object

A management unit must (UG) can only have a single description.
Therefore, all units of analysis within the same UG must include the
same descriptive elements. This function analyzes the consistency of the
descriptive elements for each management unit, and marks each row with a
logical flag `ug_valid` indicating whether it is consistent with the
dominant description of the UG.

## Usage

``` r
ua_check_ug(ua, verbose = TRUE)
```

## Arguments

- ua:

  `sf` object containing analysis units; with at least the UG identifier
  field and relevant attribute fields.

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

An `sf` object identical to `ua`, with an additional logical column
`ug_valid` indicating if each row is consistent with the dominant UG
description.

## Details

The dominant description corresponds to the one with the largest surface
area share.
