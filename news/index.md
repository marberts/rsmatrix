# Changelog

## rsmatrix 0.2.11

CRAN release: 2026-08-01

- Now using {litedown} and {tinytest}. No user-facing changes.

## rsmatrix 0.2.10

CRAN release: 2026-05-08

- Fixed failing check on CRAN.

## rsmatrix 0.2.9

CRAN release: 2024-12-14

- Updated maintainer email.

- [`rs_pairs()`](https://marberts.github.io/rsmatrix/reference/rs_pairs.md)
  gets a new argument `match_first` to control if products in the first
  period match to themselves
  ([\#1](https://github.com/marberts/rsmatrix/issues/1)).

## rsmatrix 0.2.8

CRAN release: 2023-11-19

- Added a vignette.

- [`rs_matrix()`](https://marberts.github.io/rsmatrix/reference/rs_matrix.md)
  is about twice as fast now.

## rsmatrix 0.2.6

CRAN release: 2023-06-01

- Updated to work with Matrix \>= 1.5-0.

## rsmatrix 0.2.3

CRAN release: 2022-03-15

- Making the `"Y"` vector with
  [`rs_matrix()`](https://marberts.github.io/rsmatrix/reference/rs_matrix.md)
  no longer gives an error with length-0 inputs and a factor with
  non-empty levels.

- [`rs_matrix()`](https://marberts.github.io/rsmatrix/reference/rs_matrix.md)
  cleans up the enclosing environment of its result.

## rsmatrix 0.2.1

CRAN release: 2022-02-13

- [`rs_pairs()`](https://marberts.github.io/rsmatrix/reference/rs_pairs.md)
  and
  [`rs_matrix()`](https://marberts.github.io/rsmatrix/reference/rs_matrix.md)
  are now faster, and less picky about their inputs for time periods.

## rsmatrix 0.2.0

CRAN release: 2021-10-08

- [`rs_pairs()`](https://marberts.github.io/rsmatrix/reference/rs_pairs.md)
  has been reworked to be much faster and more general, while
  `rs_unpair()` has been removed. These changes are not backwards
  compatible.

- Added French translations.

- Fixed the NOTE about LazyData from CRAN.
