
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rsequoia2 <img src="man/figures/Rsequoia2_logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/SequoiApp/Rsequoia2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mucau/Rsequoia2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of Rsequoia2 is to help french forest manager by retrieving and
creating spatial data

## Installation

You can install the development version of Rsequoia2 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SequoiApp/Rsequoia2")
```

## Quick execution

For a quick package execution, try:

``` r
library(Rsequoia2)
path <- sequoia()
```

The `sequoia()` function launches the main interactive workflow for
`Rsequoia2`.
