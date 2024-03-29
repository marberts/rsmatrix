#' Stata's degrees-of-freedom correction (internal)
#' @noRd
sss <- function(n, k, g) {
  g / (g - 1L) * (n - 1L) / (n - k)
}

#' Robust variance matrix for repeat-sales indexes
#'
#' Convenience function to compute a cluster-robust variance matrix for a
#' linear regression, with or without instruments, where clustering occurs
#' along one dimension. Useful for calculating a variance matrix when a
#' regression is calculated manually.
#'
#' This function calculates the standard robust variance matrix for a linear
#' regression, as in Manski (1988, section 8.1.2) or White (2001, Theorem 6.3);
#' that is, \eqn{(Z'X)^{-1} V (X'Z)^{-1}}{(Z'X)^-1 V (X'Z)^-1}. It is useful
#' when a regression is calculated by hand. This generalizes the variance
#' matrix proposed by Shiller (1991, section II) when a property sells more
#' than twice.
#'
#' This function gives the same result as `vcovHC(x, type = 'sss', cluster
#' = 'group')` from the \pkg{plm} package.
#'
#' @param u An \eqn{n \times 1}{n x 1} vector of residuals from a linear
#' regression.
#' @param Z An \eqn{n \times k}{n x k} matrix of instruments.
#' @param X An \eqn{n \times k}{n x k} matrix of covariates.
#' @param ids A factor of length \eqn{n}, or something that can be coerced into
#' one, that groups observations in `u`. By default each observation
#' belongs to its own group.
#' @param df An optional degrees of freedom correction. Default is Stata's
#' small sample degrees of freedom correction.
#'
#' @returns
#' A \eqn{k \times k}{k x k} covariance matrix.
#'
#' @references
#' Manski, C. (1988). *Analog Estimation Methods in Econometrics*.
#' Chapman and Hall.
#'
#' Shiller, R. J. (1991). Arithmetic repeat sales price estimators.
#' *Journal of Housing Economics*, 1(1):110-126.
#'
#' White, H. (2001). *Asymptotic Theory for Econometricians* (revised
#' edition). Emerald Publishing.
#'
#' @examples
#' # Makes some groups in mtcars
#' mtcars$clust <- letters[1:4]
#'
#' # Matrices for regression
#' x <- model.matrix(~ cyl + disp, mtcars)
#' y <- matrix(mtcars$mpg)
#'
#' # Regression coefficients
#' b <- solve(crossprod(x), crossprod(x, y))
#'
#' # Residuals
#' r <- y - x %*% b
#'
#' # Robust variance matrix
#' vcov <- rs_var(r, x, ids = mtcars$clust)
#'
#' \dontrun{
#' # Same as plm
#' library(plm)
#' mdl <- plm(mpg ~ cyl + disp, mtcars, model = "pooling", index = "clust")
#' vcov2 <- vcovHC(mdl, type = "sss", cluster = "group")
#' vcov - vcov2
#' }
#'
#' @export rs_var
#' @importMethodsFrom Matrix solve crossprod tcrossprod
rs_var <- function(u, Z, X = Z, ids = seq_len(nrow(X)), df = NULL) {
  ids <- as.factor(ids)
  df <- if (is.null(df)) {
    sss(nrow(X), ncol(X), nlevels(ids))
  } else {
    as.numeric(df)
  }
  # the meat
  ug <- split.data.frame(u, ids)
  Zg <- split.data.frame(Z, ids)
  V <- Map(function(x, y) tcrossprod(crossprod(x, y)), Zg, ug)
  V <- Reduce(`+`, V)
  # the bread
  B <- solve(crossprod(Z, X))
  # put the sandwich together
  vcov <- tcrossprod(B %*% V, B)
  df * vcov
}
