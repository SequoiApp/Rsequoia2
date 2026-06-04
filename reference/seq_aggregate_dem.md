# Prepare DEM for terrain derivatives

Aggregates a DEM to a coarser working resolution before computing
terrain derivatives such as slope or aspect.

## Usage

``` r
seq_aggregate_dem(dem, agg = 5, verbose = TRUE)
```

## Arguments

- dem:

  SpatRaster; DEM/MNT raster.

- agg:

  numeric(1); Target working resolution in meters. If NULL, no
  aggregation is applied.

- verbose:

  logical(1); If TRUE, display messages.

## Value

A SpatRaster, unchanged or aggregated.
