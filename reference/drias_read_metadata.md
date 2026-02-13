# Parse DRIAS metadata from a text file

Extracts structured metadata from a DRIAS text file, including
extraction information, climate scenario, horizons and indices.

## Usage

``` r
drias_read_metadata(txt)
```

## Arguments

- txt:

  `character` string. Path to a DRIAS `.txt` file.

## Value

A `list` with the following elements:

- date_extraction:

  `character` string. Extraction date.

- producteur:

  `character` string. Data producer.

- experience:

  `character` string. Climate experiment.

- modele:

  `character` string. Climate model.

- scenario:

  `list` with `code` and `description`.

- horizons:

  `data.frame` with columns `code` and `libelle`.

- indices:

  `data.frame` with columns `code` and `description`.
