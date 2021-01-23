
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Matrices for repeat-sales price indexes

<!-- Badges -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rsmatrix)](https://cran.r-project.org/package=rsmatrix)
[![Build
Status](https://travis-ci.org/marberts/rsmatrix.svg?branch=master)](https://travis-ci.org/marberts/rsmatrix)
[![codecov](https://codecov.io/gh/marberts/rsmatrix/branch/master/graph/badge.svg)](https://codecov.io/gh/marberts/rsmatrix)
[![rdocumentation](https://www.rdocumentation.org/badges/version/rsmatrix)](https://www.rdocumentation.org/packages/rsmatrix)

A small package for calculating the matrices in Shiller (1991) that
serve as the foundation for many repeat-sales price indexes. Builds on
the ‘rsi’ package by Kirby-McGregor and Martin (2019).

## Installation

``` r
install.package("rsmatrix")
```

Get the development version from GitHub.

``` r
devtools::install_github("marberts/rsmatrix")
```

## Usage

``` r
library(rsmatrix)

# Make some data
sales <- data.frame(id = c(1, 1, 1, 2, 2), 
                    date = c(1, 2, 3, 1, 3), 
                    price = c(1, 3, 2, 1, 1))

# Turn into sales pairs
(sales_pairs <- rs_pair(sales))
```

    ##   id date price date_prev price_prev
    ## 1  1    2     3         1          1
    ## 2  1    3     2         2          3
    ## 3  2    3     1         1          1

``` r
# Calculate matrices
matrix_constructor <- with(sales_pairs, 
                           rs_matrix(date, date_prev, price, price_prev))
matrices <- lapply(setNames(nm = c("Z", "X", "y", "Y")), matrix_constructor)

matrices$Z
```

    ##    2 3
    ## 1  1 0
    ## 2 -1 1
    ## 3  0 1

``` r
matrices$X
```

    ##    2 3
    ## 1  3 0
    ## 2 -3 2
    ## 3  0 1

``` r
# Calculate the GRS index in Bailey, Muth, and Nourse (1963)
b <- with(matrices, solve(crossprod(Z), crossprod(Z, y))[, 1])
(grs <- exp(b) * 100)
```

    ##        2        3 
    ## 238.1102 125.9921

``` r
# Calculate the ARS index in Shiller (1991)
b <- with(matrices, solve(crossprod(Z, X), crossprod(Z, Y))[, 1])
(ars <- 100 / b)
```

    ##        2        3 
    ## 240.0000 133.3333

## References

Kirby-McGregor, M., and Martin, S. (2019). An R package for calculating
repeat-sale price indices. *Romanian Statistical Review*, 3:17-33.

Shiller, R. J. (1991). Arithmetic repeat sales price estimators.
*Journal of Housing Economics*, 1(1):110-126.
