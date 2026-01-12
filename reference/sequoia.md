# Rsequoia2 Main Interactive Workflow

Launches the main interactive workflow for Rsequoia2, allowing the user
to select a project folder and choose among map layer creation or
cartographic tools.

## Usage

``` r
sequoia(path = NULL, overwrite = FALSE)
```

## Arguments

- path:

  Character. Optional. Path to the project folder. If not provided, the
  user will be prompted to enter it interactively.

## Value

Invisibly returns `path`. The function primarily calls other Rsequoia2
functions.

## Details

The function opens interactive menus to guide the user through tasks
such as:

- Creating map layers (MATRICE, PARCA, UA, UA finalization)

- Cartographic tools (currently not implemented)

The user must make selections at each step; cancelling or leaving a
selection empty will stop the function.
