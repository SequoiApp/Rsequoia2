# Generate GPU layers for a Sequoia project

Retrievesapplicable GPU (Geoportail de l’Urbanisme) layers intersecting
and surrounding the project area, and writes the resulting layer to
disk.

## Usage

``` r
seq_gpu(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  Character. Root directory of the project. Defaults to the current
  directory.

- verbose:

  Logical. Whether to display progress messages. Defaults to `TRUE`.

- overwrite:

  Logical. Whether to overwrite existing output files. Defaults to
  `FALSE`.

## Value

Invisibly returns a named list of file paths corresponding to the GPU
layers written to disk. Layers with no intersecting features are not
included.

## Details

The function queries the GPU API via the `happign` package for the
following thematic layers:

- Municipality boundaries

- Urban planning documents

- Urban zones

- Surface, linear and point prescriptions

- SUP assiettes (SUPA)

- SUP generateurs (SUPG)

Only layers intersecting the project area are written. Empty or
unavailable layers are silently skipped.

Output file names, formats and locations are fully driven by the project
configuration (see `files_structure.yaml`).

## See also

[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md),
[`happign::get_apicarto_gpu()`](https://paul-carteron.github.io/happign/reference/get_apicarto_gpu.html)
