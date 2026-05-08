# Sales pairs

Turn repeat-sales data into sales pairs that are suitable for making
repeat-sales matrices.

## Usage

``` r
rs_pairs(period, product, match_first = TRUE)
```

## Arguments

- period:

  A vector that gives the time period for each sale. Usually a date
  vector, or a factor with the levels in chronological order, but other
  values are possible if they can be sorted in chronological order
  (i.e., with [`order()`](https://rdrr.io/r/base/order.html)).

- product:

  A vector that gives the product identifier for each sale. Usually a
  factor or vector of integer codes for each product.

- match_first:

  Should products in the first period match with themselves (the
  default)?

## Value

A numeric vector of indices giving the position of the previous sale for
each `product`, with the convention that the previous sale for the first
sale is itself if `match_first = TRUE`, `NA` otherwise. Ties are
resolved according to the order they appear in `period`.

## Note

[`order()`](https://rdrr.io/r/base/order.html) is the workhorse of
`rs_pairs()`, so performance can be sensitive to the types of `period`
and `product`, and can be slow for large character vectors.

## See also

[`rs_matrix()`](https://marberts.github.io/rsmatrix/reference/rs_matrix.md)
for using sales pairs to make a repeat-sales index.

`rtCreateTrans()` in the hpiR package for a feature-rich but slower and
less flexible function to make sales pairs.

## Examples

``` r
# Make sales pairs
x <- data.frame(
  id = c(1, 1, 1, 3, 2, 2, 3, 3),
  date = c(1, 2, 3, 2, 1, 3, 4, 1),
  price = c(1, 3, 2, 3, 1, 1, 1, 2)
)

pairs <- rs_pairs(x$date, x$id)

x[c("date_prev", "price_prev")] <- x[c("date", "price")][pairs, ]

x
#>   id date price date_prev price_prev
#> 1  1    1     1         1          1
#> 2  1    2     3         1          1
#> 3  1    3     2         2          3
#> 4  3    2     3         1          2
#> 5  2    1     1         1          1
#> 6  2    3     1         1          1
#> 7  3    4     1         2          3
#> 8  3    1     2         1          2
```
