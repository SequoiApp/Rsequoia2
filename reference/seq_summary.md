# Summarize information for a Sequoia folder

Generate synthetic table from Sequoia layers and write them as `.xlsx`
inside the current Sequoia dir. Useful for redaction of management
document.

## Usage

``` r
seq_summary(dirname = ".", verbose = TRUE, overwrite = FALSE)
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

`list` of `data.frame`

## Details

Available tables:

- `"CAD"`: Cadastral parcels

- `"CAD_COM"`: Surfaces by communes

- `"PF"`: Surfaces by forest parcels

- `"SSPF"`: Surfaces by forest sub-parcels

- `"CAD_PLT"`: Link between cadastral parcels and forest parcels

- `"OCCUPATION"`: Surfaces by land use

- `"STATION"`: Surfaces by station

- `"GEOL_BDCHARM50"`: Surfaces by geology (source: BDCHARM50)

- `"GEOL_CARHAB"`: Surfaces by geology (source: CARHAB)

- `"PEDO"`: Surfaces by pedology type

- `"PLT_PF"`: Link between stand type and forest parcels

- `"PF_PLT"`: Link between forest parcels and stand type

- `"GESTION"`: Surfaces by management type

- `"ALTI_PF"`: Altimetry recap by forest parcels (max, min, mean)

- `"EXPO_PF"`: Exposition recap by forest parcels

- `"PENTE_PF"`: Slope recap by forest parcels
