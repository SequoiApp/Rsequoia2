# Parse a French cadastral property record (RP) PDF

`parse_rp()` extracts and parses non-built parcels from a French
cadastral property record PDF (Relevé de Propriété – RP).

## Usage

``` r
parse_rp(pdf)
```

## Arguments

- pdf:

  Path to a cadastral RP PDF file.

## Value

A data frame with one row per parcel (main and detail). Columns include
parcel identifiers, land use, surface (in hectares), and administrative
codes.

## Details

The function reads the PDF, identifies main parcels and detail rows,
computes parcel surfaces, and returns a tidy data frame ready for
analysis.

Only the *non-built* section of the RP is parsed. Image-only (scanned)
PDFs are not supported.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- parse_rp("releve_propriete.pdf")
head(df)
} # }
```
