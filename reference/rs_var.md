# Robust variance matrix for repeat-sales indexes

Convenience function to compute a cluster-robust variance matrix for a
linear regression, with or without instruments, where clustering occurs
along one dimension. Useful for calculating a variance matrix when a
regression is calculated manually.

## Usage

``` r
rs_var(u, Z, X = Z, ids = seq_len(nrow(X)), df = NULL)
```

## Arguments

- u:

  An \\n \times 1\\ vector of residuals from a linear regression.

- Z:

  An \\n \times k\\ matrix of instruments.

- X:

  An \\n \times k\\ matrix of covariates.

- ids:

  A factor of length \\n\\, or something that can be coerced into one,
  that groups observations in `u`. By default each observation belongs
  to its own group.

- df:

  An optional degrees of freedom correction. Default is Stata's small
  sample degrees of freedom correction.

## Value

A \\k \times k\\ covariance matrix.

## Details

This function calculates the standard robust variance matrix for a
linear regression, as in Manski (1988, section 8.1.2) or White (2001,
Theorem 6.3); that is, \\(Z'X)^{-1} V (X'Z)^{-1}\\. It is useful when a
regression is calculated by hand. This generalizes the variance matrix
proposed by Shiller (1991, section II) when a property sells more than
twice.

This function gives the same result as
`vcovHC(x, type = 'sss', cluster = 'group')` from the plm package.

## References

Manski, C. (1988). *Analog Estimation Methods in Econometrics*. Chapman
and Hall.

Shiller, R. J. (1991). Arithmetic repeat sales price estimators.
*Journal of Housing Economics*, 1(1):110-126.

White, H. (2001). *Asymptotic Theory for Econometricians* (revised
edition). Emerald Publishing.

## Examples

``` r
# Makes some groups in mtcars
mtcars$clust <- letters[1:4]

# Matrices for regression
x <- model.matrix(~ cyl + disp, mtcars)
y <- matrix(mtcars$mpg)

# Regression coefficients
b <- solve(crossprod(x), crossprod(x, y))

# Residuals
r <- y - x %*% b

# Robust variance matrix
vcov <- rs_var(r, x, ids = mtcars$clust)

if (FALSE) { # \dontrun{
# Same as plm
library(plm)
mdl <- plm(mpg ~ cyl + disp, mtcars, model = "pooling", index = "clust")
vcov2 <- vcovHC(mdl, type = "sss", cluster = "group")
vcov - vcov2
} # }
```
