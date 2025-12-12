# Download, enrich and write cadastral geometries

This function reads the `matrice.xlsx` file from a project directory,
downloads the geometry of each parcel, enriches the dataset with
information from the matrice, and writes the resulting layer to disk.

## Usage

``` r
seq_parca(dirname = ".", bdp_geom = TRUE, verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Path to the directory. Defaults to the current working
  directory.

- bdp_geom:

  `logical` If `TRUE`, replace Etalab geometries with BDP when possible.

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

**`bdp_geom`** The use and behaviour of `bdp_geom` are described in
[`get_parca()`](https://mucau.github.io/Rsequoia2/reference/get_parca.md).

**Automatic "lieu-dit" completion** If the function detects rows in the
matrice where the field `"LIEU_DIT"` is missing, the corresponding
"lieu-dit" values will be downloaded automatically and added to the
output. Existing `"LIEU_DIT"` values are **never overwritten**.
