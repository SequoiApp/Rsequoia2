# Check cadastral IDU consistency between \_UA\_ and \_PARCA\_

Checks that all \_PARCA\_ IDU values are present in \_UA\_, and that all
\_UA\_ IDU values exist in \_PARCA\_.

## Usage

``` r
ua_check_idu(ua, parca, verbose = FALSE)
```

## Arguments

- ua:

  \`sf\` Object containing analysis units.

- parca:

  \`sf\` Object containing cadastral parcels.

- verbose:

  \`logical\` If \`TRUE\`, display messages.

## Value

\`TRUE\` if IDU values are consistent; \`FALSE\` otherwise, with CLI
warnings.
