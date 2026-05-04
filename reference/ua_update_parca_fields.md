# Update *UA* fields from *PARCA*

Refreshes *UA* cadastral fields from *PARCA* using the IDU field as key.

## Usage

``` r
ua_update_parca_fields(ua, parca)
```

## Arguments

- ua:

  `sf` Object containing analysis units.

- parca:

  `sf` Object containing cadastral parcels.

## Value

A `sf` *UA* object with cadastral fields updated from *PARCA*.

## Details

This function assumes that *UA* IDU values are already correct.
