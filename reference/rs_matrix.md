# Shiller's repeat-sales matrices

Create a function to compute the \\Z\\, \\X\\, \\y\\, and \\Y\\ matrices
in Shiller (1991, sections I-II) from sales-pair data in order to
calculate a repeat-sales price index.

## Usage

``` r
rs_matrix(t2, t1, p2, p1, f = NULL, sparse = FALSE)
```

## Arguments

- t2, t1:

  `[Date | character]` A pair of vectors giving the time period of the
  second and first sale, respectively. Usually a vector of dates, but
  other values are possible if they can be coerced to character vectors
  and sorted in chronological order (i.e., with
  [`order()`](https://rdrr.io/r/base/order.html)).

- p2, p1:

  `[numeric > 0]` A pair of numeric vectors giving the price of the
  second and first sale, respectively.

- f:

  `[factor]` An optional factor the same length as `t1` and `t2`, or a
  vector to be turned into a factor, that is used to group sales.

- sparse:

  `[logical(1)]` Should sparse matrices from the Matrix package be used
  (faster for large datasets), or regular dense matrices (the default)?

## Value

A function that takes a single argument naming the desired matrix. It
returns one of two matrices (\\Z\\ and \\X\\) or two vectors (\\y\\ and
\\Y\\), either regular matrices if `sparse = FALSE`, or sparse matrices
of class `dgCMatrix` if `sparse = TRUE`.

## Details

The function returned by `rs_matrix()` computes a generalization of the
matrices in Shiller (1991, sections I-II) that are applicable to grouped
data. These are useful for calculating separate indexes for many, say,
cities without needing an explicit loop.

The \\Z\\, \\X\\, and \\Y\\ matrices are not well defined if either `t1`
or `t2` have missing values, and an error is thrown in this case.
Similarly, it should always be the case that `t2 > t1`, otherwise a
warning is given.

## References

Bailey, M. J., Muth, R. F., and Nourse, H. O. (1963). A regression
method for real estate price index construction. *Journal of the
American Statistical Association*, 53(304):933-942.

Shiller, R. J. (1991). Arithmetic repeat sales price estimators.
*Journal of Housing Economics*, 1(1):110-126.

## See also

[`rs_pairs()`](https://marberts.github.io/rsmatrix/reference/rs_pairs.md)
for turning sales data into sales pairs.

## Examples

``` r
# Make some data
x <- data.frame(
  date = c(3, 2, 3, 2, 3, 3),
  date_prev = c(1, 1, 2, 1, 2, 1),
  price = 6:1,
  price_prev = 1
)

# Calculate matrices
mat <- with(x, rs_matrix(date, date_prev, price, price_prev))
Z <- mat("Z") # Z matrix
X <- mat("X") # X matrix
y <- mat("y") # y vector
Y <- mat("Y") # Y vector

# Calculate the GRS index in Bailey, Muth, and Nourse (1963)
b <- solve(crossprod(Z), crossprod(Z, y))[, 1]
# or b <- qr.coef(qr(Z), y)
(grs <- exp(b) * 100)
#>        2        3 
#> 235.0755 403.5654 

# Standard errors
vcov <- rs_var(y - Z %*% b, Z)
sqrt(diag(vcov)) * grs # delta method
#>        2        3 
#> 111.0797 257.6581 

# Calculate the ARS index in Shiller (1991)
b <- solve(crossprod(Z, X), crossprod(Z, Y))[, 1]
# or b <- qr.coef(qr(crossprod(Z, X)), crossprod(Z, Y))
(ars <- 100 / b)
#>        2        3 
#> 310.5263 491.6667 

# Standard errors
vcov <- rs_var(Y - X %*% b, Z, X)
sqrt(diag(vcov)) * ars^2 / 100 # delta method
#>        2        3 
#> 100.0316 232.3111 

# Works with grouped data
x <- data.frame(
  date = c(3, 2, 3, 2),
  date_prev = c(2, 1, 2, 1),
  price = 4:1,
  price_prev = 1,
  group = c("a", "a", "b", "b")
)

mat <- with(x, rs_matrix(date, date_prev, price, price_prev, group))
b <- solve(crossprod(mat("Z"), mat("X")), crossprod(mat("Z"), mat("Y")))[, 1]
100 / b
#>  a.2  b.2  a.3  b.3 
#>  300  100 1200  200 
```
