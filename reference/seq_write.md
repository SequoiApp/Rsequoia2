# Write a spatial object based on a layer key

Write a spatial object based on a layer key

## Usage

``` r
seq_write(x, key, dirname = ".", verbose = FALSE, overwrite = FALSE)
```

## Arguments

- x:

  An `sf` object (for vector outputs) or a `SpatRaster` (for raster
  outputs).

- key:

  `character` Name of a layer key to match against the entries defined
  in `inst/config/seq_layers.yaml`. (see *Details* for partial
  matching).

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

Invisibly returns the filepath used for writing.
