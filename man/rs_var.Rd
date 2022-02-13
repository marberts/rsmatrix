\name{rs_var}
\alias{rs_var}
\title{Robust variance matrix for repeat-sales indexes}

\description{
Compute a cluster-robust variance matrix for a linear regression, with or without instruments, where clustering occurs along one dimension. Useful for calculating a variance matrix when a regression is calculated manually.
}

\usage{
rs_var(u, Z, X = Z, ids = seq_len(nrow(X)), df = NULL)
}

\arguments{
\item{u}{An \eqn{n \times 1}{n x 1} vector of residuals from a linear regression.}

\item{Z}{An \eqn{n \times k}{n x k} matrix of instruments.}

\item{X}{An  \eqn{n \times k}{n x k} matrix of covariates.}

\item{ids}{A factor of length \eqn{n}{n}, or something that can be coerced into one, that groups observations in \code{u}. By default each observation belongs to its own group.}

\item{df}{An optional degrees of freedom correction. Default is Stata's small sample degrees of freedom correction.}
}

\details{
This function calculates the standard robust variance matrix for a linear regression, as in Manski (1988, section 8.1.2) or White (2001, Theorem 6.3); that is, \eqn{(Z'X)^{-1} V (X'Z)^{-1}}{(Z'X)^-1 V (X'Z)^-1}. It is useful when a regression is calculated by hand. This generalizes the variance matrix proposed by Shiller (1991, section II) when a property sells more than twice.

This function gives the same result as \code{vcovHC(x, type = 'sss', cluster = 'group')} from the \pkg{plm} package.
}

\value{
A \eqn{k \times k}{k x k} covariance matrix.
}

\references{
Manski, C. (1988). \emph{Analog Estimation Methods in Econometrics}. Chapman and Hall.

Shiller, R. J. (1991). Arithmetic repeat sales price estimators. \emph{Journal of Housing Economics}, 1(1):110-126.

White, H. (2001). \emph{Asymptotic Theory for Econometricians} (revised edition). Emerald Publishing.
}

\examples{
# Makes some groups in mtcars
mtcars$clust<- letters[1:4]

# Matrices for regression
x <- model.matrix(~ cyl + disp, mtcars)
y <- matrix(mtcars$mpg)

# Regression coefficients
b <- solve(crossprod(x), crossprod(x, y))

# Residuals
r <- y - x \%*\% b

# Robust variance matrix
vcov <- rs_var(r, x, ids = mtcars$clust)

\dontrun{
# Same as plm
library(plm)
mdl <- plm(mpg ~ cyl + disp, mtcars, model = 'pooling', index = 'clust')
vcov2 <- vcovHC(mdl, type = 'sss', cluster = 'group')
vcov - vcov2
}
}