# Generate access point layer for a Sequoia project

Create an empty \`sf\` for access point features, and writes the
resulting layer to disk.

## Usage

``` r
seq_access(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  \`character\` Path to the project directory. Defaults to the current
  working directory.

- verbose:

  \`logical\`; whether to display informational messages. Defaults to
  \`TRUE\`.

- overwrite:

  \`logical\`; whether to overwrite existing files. Defaults to
  \`FALSE\`.

## Value

Invisibly returns a named list of file paths written by \[seq_write()\].

## Details

The access point layer is an empty layer : user must point access
themselves. The layer is written to disk using \[seq_write()\] with the
key \`"v.access.entry.point"\`.

## See also

\[seq_write()\]
