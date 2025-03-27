## rsmatrix 0.2.9

- Updated maintainer email.

- `rs_pairs()` gets a new argument `match_first` to control if products in the
first period match to themselves (#1).

## rsmatrix 0.2.8

- Added a vignette.

- `rs_matrix()` is about twice as fast now.

## rsmatrix 0.2.6

- Updated to work with Matrix >= 1.5-0.

## rsmatrix 0.2.3

- Making the `"Y"` vector with `rs_matrix()` no longer gives an error with
length-0 inputs and a factor with non-empty levels.

- `rs_matrix()` cleans up the enclosing environment of its result.

## rsmatrix 0.2.1

- `rs_pairs()` and `rs_matrix()` are now faster, and less picky about their
inputs for time periods.

## rsmatrix 0.2.0

- `rs_pairs()` has been reworked to be much faster and more general,
while `rs_unpair()` has been removed. These changes are not backwards compatible.

- Added French translations.

- Fixed the NOTE about LazyData from CRAN.
