# Create terrain derivative layers for a Sequoia project

Uses the project's MNT raster to compute slope and aspect rasters, then
writes them to the project directory with \[seq_write()\].

## Usage

``` r
seq_terrain(
  dirname = ".",
  agg = 5,
  unit = "percent",
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dirname:

  \`character\` Directory where the matrice file is located. Defaults to
  the current working directory.

- agg:

  \`numeric\`; Target resolution (in meters) to which the DEM is
  aggregated if its native resolution is finer. Default: \`5\`.

- unit:

  \`character\`; "percent", "radians" or "degrees". Default to
  "percent".

- overwrite:

  \`logical\` If \`TRUE\`, file is overwritten.

- verbose:

  \`logical\`; If \`TRUE\`, display messages.

## Value

Invisibly returns a named \`character\` vector of output raster paths.

## See also

\[get_slope()\], \[get_aspect()\]
