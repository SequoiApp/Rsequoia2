# Check and update the analysis units layer

This function reads the analysis units layer file from a project
directory, check and udapte attributes and overwrite it.

## Usage

``` r
seq_ua(dirname = ".", secure = TRUE, verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- secure:

  `logical`. If `TRUE`, also writes a timestamped secure copy of the
  file.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

An `sf` object

## Details

The resulting object is returned invisibly as an `sf` polygons layer.
The output file is automatically written into the working directory
defined by `dirname`.
