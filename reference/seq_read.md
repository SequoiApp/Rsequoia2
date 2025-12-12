# Open file based on key name

Open file based on key name

## Usage

``` r
seq_read(key, dirname = ".", verbose = FALSE)
```

## Arguments

- key:

  `character` Name of a layer key to match against the entries defined
  in `inst/config/seq_layers.yaml`. (see *Details* for partial
  matching).

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

Object of class `SpatRaster` for raster and `sf` for vector
