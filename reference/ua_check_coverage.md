# Check spatial coverage between \_UA\_ and \_PARCA\_

Checks that every \_UA\_ feature intersects at least one \_PARCA\_
feature, and that every \_PARCA\_ feature intersects at least one \_UA\_
feature.

## Usage

``` r
ua_check_coverage(ua, parca)
```

## Arguments

- ua:

  \`sf\` Object containing analysis units.

- parca:

  \`sf\` Object containing cadastral parcels.

## Value

The input \`ua\` object, invisibly, if spatial coverage is valid. Aborts
otherwise.
