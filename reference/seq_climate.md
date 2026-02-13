# Generate meteorological outputs

Downloads and formats meteorological data from Météo-France and DRIAS.
This includes download of the nearest climatological station fiche
(PDF), ombrothermic summaries, precipitation statistics. Results are
written to an Excel workbook based on the internal template
`CLIMAT_MF.xlsx`.

## Usage

``` r
seq_climate(dirname = ".", cache = NULL, verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- cache:

  `character`; Storage directory. Defaults to the user cache directory
  (see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html))

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

A named `list` of file paths :

- \`"fiche.meteo"“: Path to the downloaded climatological station PDF

- \`"meteofrance"“: Path to the generated Excel workbook
