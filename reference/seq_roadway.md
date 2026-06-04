# Generate graphic road layers for a Sequoia project

Create graphic road layers from a completed cadastral gaps layer.

## Usage

``` r
seq_roadway(source, dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- source:

  \`character\` Source layer key to use to generate polygon features.
  Must be \`v.cad.vides.poly\` or \`v.road.line\`.

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
Returns \`NULL\` invisibly when no features are created.

## Details

Features are created from a completed cadastral gaps layer obtained by
using \`seq_voids()\`.

## See also

\[seq_voids()\], \[seq_write()\]
