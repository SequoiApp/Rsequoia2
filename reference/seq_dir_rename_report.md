# Print Sequoia directory rename report

Print a compact `cli` report after renaming files and updating
GeoPackages.

## Usage

``` r
seq_dir_rename_report(renamed, report)
```

## Arguments

- renamed:

  Logical vector returned by
  [`seq_files_rename()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_files_rename.md).

- report:

  Data frame returned by
  [`seq_dir_rename()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_dir_rename.md).

## Value

Invisibly returns `NULL`.
