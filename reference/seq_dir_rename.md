# Rename a Sequoia directory

Rename files in a Sequoia directory by replacing an old forest
identifier with a new one, then update the Sequoia identifier field
inside all GeoPackage layers found in the directory.

## Usage

``` r
seq_dir_rename(path, old_id, new_id, verbose = TRUE)
```

## Arguments

- path:

  Character. Path to the Sequoia directory.

- old_id:

  Character. Identifier to replace in file names.

- new_id:

  Character. New identifier to use in file names and GeoPackage
  attributes.

- verbose:

  Logical. If `TRUE`, print a console report with `cli`.

## Value

Invisibly returns a data frame with one row per GeoPackage and the
columns `file`, `status`, and `message`.

## Details

The function is fault-tolerant: if one GeoPackage cannot be read or
updated, the error is reported but the remaining files are still
processed.

## Examples

``` r
if (FALSE) { # \dontrun{
seq_dir_rename(
  path = "C:/Users/PaulCarteron/Desktop/3233B_LA MOUSSAYE",
  old_id = "3233B",
  new_id = "LA MOUSSAYE"
)
} # }
```
