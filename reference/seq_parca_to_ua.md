# Create analysis units layer

This function reads the cadastral layer file from a project directory
and create the analysis units layer.

## Usage

``` r
seq_parca_to_ua(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Path to the directory. Defaults to the current working
  directory.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, filename is overwritten.

## Value

An `sf` object

## Details

The resulting object is returned invisibly as an `sf` polygons layer.
The output file is automatically written into the working directory
defined by `dirname`.
