# Repair \_UA\_ cadastral IDU from \_PARCA\_

Repairs \_UA\_ IDU values from the dominant intersecting \_PARCA\_
polygon.

## Usage

``` r
ua_repair_idu(ua, parca, verbose = TRUE)
```

## Arguments

- ua:

  \`sf\` Object containing analysis units.

- parca:

  \`sf\` Object containing cadastral parcels.

- verbose:

  \`logical\` If \`TRUE\`, display messages.

## Value

The input \`ua\` object with corrected IDU values.

## Details

The spatial match is based on \[sf::st_join()\] with \`largest = TRUE\`,
so the \_PARCA\_ polygon with the largest overlap is used as reference.
