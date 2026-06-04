# Create \_PARCA\_ sf object from \_UA\_ sf object

Create \_PARCA\_ sf object from \_UA\_ sf object

## Usage

``` r
ua_to_parca(ua)
```

## Arguments

- ua:

  \`sf\` object containing analysis units

## Value

An \`sf\` object containing cadastral parcels.

## Details

This function deviates from the traditional process. It allows the
\_PARCA\_ layer to be recreated from a \_UA\_ layer in the case of a
folder where the first layer is missing.
