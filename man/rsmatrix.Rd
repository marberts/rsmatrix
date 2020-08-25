\docType{package}
\name{rsmatrix-package}
\alias{rsmatrix-package}
\alias{rsmatrix}

\title{Matrices for Repeat-Sales Price Indexes}

\description{
A small package for calculating the matrices in Shiller (1991) that serve as the foundation for many repeat-sales price indexes.
}

\details{
Most repeat-sales price indexes used in practice require the matrices in Shiller (1991, sections I-II), e.g., S&P's Case-Shiller index, Statistics Canada's RPPI. The \code{\link[rsmatrix:rs_matrix]{rs_matrix()}} function produces a function to construct these matrices. In most cases data need to be structured as sales pairs, which can be done with the \code{\link[rsmatrix:rs_pair]{rs_pair()}} function.
}

\note{
The \code{'McSpatial'} package has some functionality for making repeat-sales indices. The functions in this package build off of those in the \code{'rsi'} package in Kirby-McGregor and Martin (2019), which also gives a good background on the theory of repeat-sales indexes.
}

\references{
ILO, IMF, OECD, UN, World Bank, Eurostat. (2013). \emph{Handbook on Residential
Property Prices Indices (RPPIs)}. Eurostat.

Kirby-McGregor, M., and Martin, S. (2019). An R package for calculating repeat-sale price indices. \emph{Romanian Statistical Review}, 3:17-33.

Shiller, R. J. (1991). Arithmetic repeat sales price estimators. \emph{Journal of Housing Economics}, 1(1):110-126.
}

\seealso{
\url{https://github.com/marberts/rsmatrix}
}

\author{
\strong{Maintainer}: Steve Martin \email{stevemartin041@gmail.com}
}