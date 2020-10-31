\name{rs_var}
\alias{rs_var}
\title{Robust variance matrix for repeat-sales index}

\description{
Compute a cluster-robust variance matrix for a linear regression, with or without instruments, where clustering occurs along one dimension. Useful for calculating a variance matrix when a regression is calculated manually.
}

\usage{
rs_var(u, Z, X = Z, ids = seq_len(nrow(X)), df = sss(nrow(X), ncol(X), distinct(ids)))
}

\arguments{
\item{u}{An nx1 vector of residuals from a linear regression.}

\item{Z}{An nxk matrix of instruments.}

\item{X}{An nxk matrix of covariates.}

\item{ids}{A vector of length n that groups observations in \code{u}. By default each observation belongs to its own group.}

\item{df}{An optional degrees of freedom correction. Default is Stata's small sample degrees of freedom correction.}
}

\value{
A kxk matrix.
}

\details{
This function calculates the standard robust variance matrix for a linear regression, as in Manski (1988, section 8.1.2) or White (2001, Theorem 6.3); that is, (Z'X)^-1 V (X'Z)^-1. It is useful when a regression is calculated by hand. This generalizes the variance matrix proposed by Shiller (1991, section II) when a property sells more than twice.

This function gives the same result as \code{vcovHC(x, type = 'sss', cluster = 'group')} from the \code{'plm'} package.
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