# Extract table section from page

Extracts the data table section from a PDF page using anchor positions.
Removes repeated column headers.

## Usage

``` r
extract_table_section(page_data, non_anchor, has_total)
```

## Arguments

- page_data:

  A `data.frame` for a single pdf page

- non_anchor:

  Anchor row from
  [`identify_non_anchors()`](https://mucau.github.io/Rsequoia2/reference/identify_non_anchors.md)

- has_total:

  `logical`, whether page contains "totale" anchor

## Value

Filtered `data.frame` containing only table rows
