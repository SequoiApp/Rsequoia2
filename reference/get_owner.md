# Extract owner information from PDF

Parses French cadastral PDFs to extract owner details including type,
name, and address from property rights sections.

## Usage

``` r
get_owner(pdf)
```

## Arguments

- pdf:

  `character`. Path to PDF file

## Value

A `data.frame` with columns: type, type_code, code, name, firstname,
address, seq_name, file_name, file_path
