# Process multiple PDFs and combine results

Applies the extraction pipeline to multiple PDF files and combines
results into a single list with three data frames.

## Usage

``` r
get_matrice(pdf_files)
```

## Arguments

- pdf_files:

  `character` vector. Paths to PDF file

## Value

List with three `data.frame`: `refs`, `owners`, `parcels`
