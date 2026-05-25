# Identify "non batie(s)" section anchors

Locates the "non" markers that indicate the start of unbuilt property
sections in French cadastral PDFs, with context validation.

## Usage

``` r
identify_non_anchors(all_visual_data)
```

## Arguments

- all_visual_data:

  A \`data.frame\` from \[extract_visual_data()\]

## Value

A \`data.frame\` of anchor positions with same columns as input
