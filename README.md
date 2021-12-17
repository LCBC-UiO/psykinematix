
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psykinematix <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/LCBC-UiO/psykinematix/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/psykinematix/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/psykinematix)](https://CRAN.R-project.org/package=psykinematix)
<!-- badges: end -->

[Psykinematix Visual
Psychophysics](https://psykinematix.kybervision.net/) is a tool to
develop and deploy psychophysical experiments. This packages enables the
user to read in data from this tool into R for further processing.

## Installation

<!-- You can install the released version of psykinematix from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("psykinematix") -->
<!-- ``` -->

The current easiest install option is through the LCBC r-universe.

``` r
# Enable universe(s) by lcbc-uio
options(repos = c(
  lcbcuio = 'https://lcbc-uio.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('psykinematix')
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/psykinematix")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(psykinematix)
#> Warning: package 'psykinematix' was built under R version 4.1.2
## basic example code
```
