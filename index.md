# Rsequoia2

## Français

`Rsequoia2` est un package R conçu pour accompagner les gestionnaires
forestiers Français.

Il permet de récupérer, préparer, corriger et synthétiser les données
spatiales utilisées dans les projets forestiers.

`Rsequoia2` est principalement pensé pour travailler avec des données
spatiales produites ou utilisées dans QGIS, notamment en complément du
plugin QGIS `Qsequoia2`.

## English

`Rsequoia2` is an R package designed to support French forest management
workflows.

It helps retrieve, prepare, clean and summarize spatial data used in
Sequoia forestry projects.

`Rsequoia2` is mainly intended to work with spatial data produced or
used in QGIS and can be used together with the `Qsequoia2` QGIS plugin.

## Installation

You can install the development version of `Rsequoia2` from GitHub with:

``` r

#| eval: false
# install.packages("pak")
pak::pak("SequoiApp/Rsequoia2")
```

## Quick start

Load the package and launch the main interactive workflow:

``` r

library(Rsequoia2)
sequoia2()
```

The
[`sequoia2()`](https://sequoiapp.github.io/Rsequoia2/reference/sequoia2.md)
function launches the main interactive menu for running the package
workflows.

## Relationship with Qsequoia2

`Rsequoia2` handles data preparation, correction, download and synthesis
workflows from R.

`Qsequoia2` is the companion QGIS plugin used to load, edit, visualize
and lay out Sequoia project layers inside QGIS.

Both tools are designed to work together in the same forestry project
workflow.

## Development status

`Rsequoia2` is currently experimental.

The package is under active development and the API may change as
workflows are improved.

## Bug reports and suggestions

Please report bugs, suggestions or workflow issues on GitHub:

``` r

utils::browseURL("https://github.com/SequoiApp/Rsequoia2/issues")
```

## Code of Conduct

Please note that the usethis project is released with a [Contributor
Code of
Conduct](https://sequoiapp.github.io/Rsequoia2/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
