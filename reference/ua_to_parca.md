# Create *PARCA* sf object from *UA* sf object

Create *PARCA* sf object from *UA* sf object

## Usage

``` r
ua_to_parca(ua)
```

## Arguments

- ua:

  `sf` object containing analysis units

## Value

An `sf` object containing cadastral parcels.

## Details

This function deviates from the traditional process. It allows the
*PARCA* layer to be recreated from a *UA* layer in the case of a folder
where the first layer is missing.
