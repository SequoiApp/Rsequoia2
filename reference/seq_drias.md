# Generate DRIAS climate projection outputs

Reads, formats, and exports climate projection data from DRIAS. The
function expects a DRIAS projection `.txt` file located in the `4_METEO`
directory of the Sequoia project. It extracts metadata, raw climatology,
ombrothermic summaries, and evapotranspiration (ETP) indicators. Results
are written to an Excel workbook based on the internal template
`CLIMAT_DRIAS.xlsx`.

## Usage

``` r
seq_drias(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

A named `character` vector:

- `"drias"`: Path to the generated Excel workbook

## Details

The function looks for a single `.txt` file in the `4_METEO` directory.
This file must be downloaded beforehand from the DRIAS portal
(https://drias-climat.fr/).
