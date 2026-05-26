# Get a border layer from polygons

This function convert polygon to line and remove shared lines

## Usage

``` r
poly_to_line(x)
```

## Arguments

- x:

  `sf POLYGONS`, using a projected CRS

## Value

An `sf` object with two `MULTILINESTRING` geometry is returned

## Note

If the polygon layer contains topology errors (such as contiguous
polygons not sharing exactly the same boundary)
