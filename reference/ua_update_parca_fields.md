# Update \_UA\_ fields from \_PARCA\_

Refreshes \_UA\_ cadastral fields from \_PARCA\_ using the IDU field as
key.

## Usage

``` r
ua_update_parca_fields(ua, parca)
```

## Arguments

- ua:

  \`sf\` Object containing analysis units.

- parca:

  \`sf\` Object containing cadastral parcels.

## Value

A \`sf\` \_UA\_ object with cadastral fields updated from \_PARCA\_.

## Details

This function assumes that \_UA\_ IDU values are already correct.
