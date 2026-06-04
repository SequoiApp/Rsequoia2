# Compute a hillshade raster from a DHM

Computes a shaded relief raster from a Digital Height Model using slope
and aspect derived directly from the DHM.

## Usage

``` r
get_shade(r, angle = 30, direction = c(225, 270, 315, 360))
```

## Arguments

- r:

  \`SpatRaster\`; Digital Height Model raster.

- angle:

  \`numeric\`; Sun elevation angle in degrees.

- direction:

  \`numeric\`; Sun direction/azimuth in degrees.

## Value

A \`SpatRaster\` containing hillshade values.
