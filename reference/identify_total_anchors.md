# Identify "totale" section anchors

Locates the "totale" markers that indicate section ends in French
cadastral PDFs, validated by proximity to "Contenance".

## Usage

``` r
identify_total_anchors(all_visual_data)
```

## Arguments

- all_visual_data:

  A `data.frame` from
  [`extract_visual_data()`](https://sequoiapp.github.io/Rsequoia2/reference/extract_visual_data.md)

## Value

A `data.frame` of anchor positions with same columns as input
