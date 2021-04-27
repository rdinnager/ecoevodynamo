
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecoevodynamo

<!-- badges: start -->

[![R-CMD-check](https://github.com/rdinnager/ecoevodynamo/workflows/R-CMD-check/badge.svg)](https://github.com/rdinnager/ecoevodynamo/actions)
[![Codecov test
coverage](https://codecov.io/gh/rdinnager/ecoevodynamo/branch/main/graph/badge.svg)](https://codecov.io/gh/rdinnager/ecoevodynamo?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `ecoevodynamo` is to make it easy to generate
ecoevolutionary dynamics simulations when you only have equations for
the ecological dynamics. `ecoevodynamo` uses automatic differentiation,
through the `torch` package, to automatically calculate selection
gradient for all traits utilised in the ecological model, and can do so
even for very complex models. It then simulates a system of ordinary
differential equation including both ecological and evolutionary
dynamics simultaneously. This makes it distinct from the related
approach of Adaptive Dynamics, which typically creates a separation
between ecological and evolutionary dynamics, assuming they take place
on completely different timescales. We now know that evolution occurs on
a timescale comparable to ecology, and this approach recognizes that.

## Installation

Currently, you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rdinnager/ecoevodynamo")
```

## Example

Still working on an example.
