# Extract visual data from PDF with page numbers

Wrapper around
[`pdftools::pdf_data()`](https://docs.ropensci.org/pdftools//reference/pdftools.html)
that adds page numbers to each extracted text element.

## Usage

``` r
extract_visual_data(pdf)
```

## Arguments

- pdf:

  `character`. Path to PDF file

## Value

A `data.frame` with columns from `pdf_data()` plus `page` (integer)

## See also

[`pdftools::pdf_data()`](https://docs.ropensci.org/pdftools//reference/pdftools.html).
