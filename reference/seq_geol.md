# Create a geology layer for a Sequoia project from BRGM data

Uses the *PARCA* from Sequoia to determine which French departments are
involved, downloads the corresponding BRGM geology datasets, and builds
a single geology layer for the project.

## Usage

``` r
seq_geol(dirname = ".", cache = NULL, verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- cache:

  `character`; Storage directory. Defaults to the user cache directory
  (see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, overwrite zipfile.

## Value

An invisible `sf` object containing the merged geology layer.

## Details

Created layer and its *QML style file* are automatically written to the
project directory using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).

**Difference between BRGM and CARHAB data**

The BRGM 1:50000 geological maps are detailed, heterogeneous products
created for geological interpretation. They include many finely split
geological units and may show inconsistencies between neighbouring map
sheets due to varying survey dates, methods, and levels of detail.

CARHAB data are a simplified, harmonised reinterpretation of these maps.
Geological formations are recoded into broader lithological classes
tailored for ecological modelling. The goal is to provide consistent,
comparable, and ecologically relevant information across departments.

In short: **BRGM layers describe geology in detail; CARHAB layers
provide a simplified lithology better suited for habitat modelling.**

More info at
[infoterre](https://infoterre.brgm.fr/page/carhab-donnees-geologiques)
