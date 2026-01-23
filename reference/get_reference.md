# Extract reference information from PDF header

Parses the first page of a French cadastral PDF to extract commune,
department, and other reference information from the header section.

## Usage

``` r
get_reference(pdf_path)
```

## Arguments

- pdf_path:

  `character`. Path to PDF file

## Value

A `data.frame` with columns: year, department_code, commune_code,
commune_name, tres, communal_num, file_name, file_path, insee
