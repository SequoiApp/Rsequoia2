# Create geology layers for a Sequoia project from BRGM data

Uses the project's *PARCA* layer to download geology datasets and clip
them to its geometry.

## Usage

``` r
seq_geol(
  dirname = ".",
  key = NULL,
  cache = NULL,
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- key:

  `character`. Optional geology layer identifier(s). If `NULL`
  (default), all available geology layers are created. Available layers
  are `"bdcharm50"` and `"carhab"`. Partial matching is supported (see
  [`seq_key()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_key.md)).

- cache:

  `character`; Storage directory. Defaults to the user cache directory
  (see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, overwrite zipfile.

## Value

An invisible named `list` of file paths to the created layers.

## Details

Layers and their associated *QML style files* are written to the project
directory using
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md).

**Difference between BRGM and CARHAB data**

The BRGM 1:50000 geological maps are detailed, heterogeneous products
created for geological interpretation. They include many finely split
geological units and may show inconsistencies between neighbouring map
sheets due to varying survey dates, methods, and levels of detail.

CARHAB data are a simplified, harmonised reinterpretation of these maps.
Geological formations are recoded into broader lithological classes
tailored for ecological modelling. The goal is to provide consistent,
comparable, and ecologically relevant information across departments.

More info at
[infoterre](https://infoterre.brgm.fr/page/carhab-donnees-geologiques)
