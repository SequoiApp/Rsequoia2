# Rename matching files

Rename files recursively by replacing `old_id` with `new_id` in file
names.

## Usage

``` r
seq_files_rename(path, old_id, new_id)
```

## Arguments

- path:

  Character. Directory path.

- old_id:

  Character. Identifier to replace.

- new_id:

  Character. Replacement identifier.

## Value

Logical vector returned by
[`file.rename()`](https://rdrr.io/r/base/files.html).
