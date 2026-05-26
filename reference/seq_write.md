# Write a spatial object based on a layer key

Write a spatial object based on a layer key

## Usage

``` r
seq_write(x, key, dirname = ".", id = NULL, verbose = FALSE, overwrite = FALSE)
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

- id:

  `character`. Project identifier used to prefix the output filename. If
  `NULL`, the identifier is automatically inferred. The function first
  attempts to extract a single, non-missing value from the `IDENTIFIANT`
  field of layer `x`; if this fails, it then looks for a valid
  identifier in a `*_matrice.xlsx` file found in `dirname`. If no unique
  identifier can be determined, the filename is left unchanged.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

Invisibly returns the filepath used for writing.
