# Create terrain derivative layers for a Sequoia project

Uses the project's MNT raster to compute slope and aspect rasters, then
writes them to the project directory with
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md).

## Usage

``` r
seq_terrain(dirname = ".", agg = 5, overwrite = FALSE, verbose = TRUE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- agg:

  `numeric`; Target working resolution in meters before terrain
  calculation. Use `NULL` to keep native resolution.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

Invisibly returns a named `character` vector of output raster paths.

## See also

[`get_slope()`](https://sequoiapp.github.io/Rsequoia2/reference/get_slope.md),
[`get_aspect()`](https://sequoiapp.github.io/Rsequoia2/reference/get_aspect.md)
