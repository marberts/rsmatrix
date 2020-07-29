\docType{package}
\name{rsmatrix-package}
\alias{rsmatrix-package}
\alias{rsmatrix}

\title{Matrices for Repeat-Sale Price Indexes}

\description{
A small package for calculating the matrices in Shiller (1991, <doi:10.1016/S1051-1377(05)80028-2>) for repeat-sale price indexes.
}

\details{
Most repeat-sales price indexes used in practice require the Z and X matrices in Shiller (1991, sections I-II). The \code{\link[rsmatrix:rs_z]{rs_z()}} and \code{\link[rsmatrix:rs_x]{rs_x()}} functions construct these matrices. In most cases data need to be structured as "sales pairs", which can be done with the \code{\link[rsmatrix:rs_pair]{rs_pair()}} function.
}

\note{
The 'mcspatial' package also has some functionality for making repeat-sales indices. The functions in this package build off of those in the 'rsi' package in Kirby-McGregor and Martin (2019).
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