# Get Météo-France Dataset Metadata

Downloads and parses the metadata describing variables from the
Météo-France monthly climate dataset ("MENSQ").

## Usage

``` r
mf_get_metadata()
```

## Value

A `data.frame` with two columns

## Details

The function retrieves the `descriptif_champs` resource from the
corresponding data.gouv.fr dataset and returns a structured table
containing variable names and their descriptions.

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- mf_get_metadata()
head(meta)
} # }
```
