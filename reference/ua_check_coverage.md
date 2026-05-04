# Check spatial coverage between *UA* and *PARCA*

Checks that every *UA* feature intersects at least one *PARCA* feature,
and that every *PARCA* feature intersects at least one *UA* feature.

## Usage

``` r
ua_check_coverage(ua, parca)
```

## Arguments

- ua:

  `sf` Object containing analysis units.

- parca:

  `sf` Object containing cadastral parcels.

## Value

The input `ua` object, invisibly, if spatial coverage is valid. Aborts
otherwise.
