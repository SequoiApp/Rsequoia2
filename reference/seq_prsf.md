# Generate PRSF point layer for a Sequoia project

Retrieves PRSF point features intersecting and surrounding the project
area, and writes the resulting layer to disk.

## Usage

``` r
seq_prsf(dirname = ".", buffer = 5000, verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  \`character\` Directory where the matrice file is located. Defaults to
  the current working directory.

- buffer:

  \`numeric\`; Buffer around \`x\` (in \*\*meters\*\*) used to enlarge

- verbose:

  \`logical\` If \`TRUE\`, display messages.

- overwrite:

  \`logical\` If \`TRUE\`, file is overwritten.

## Value

Invisibly returns a named list of file paths written by \[seq_write()\].
Returns \`NULL\` invisibly when no PRSF point features are found.

## Details

PRSF point features are retrieved using \[get_prsf()\].

If no PRSF point features are found, the function returns \`NULL\`
invisibly and no file is written.

When features are present, the layer is written to disk using
\[seq_write()\] with the key \`"v.prsf.point"\`.

## See also

\[get_prsf()\], \[seq_write()\]
