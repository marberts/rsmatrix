\name{rs_matrix}
\alias{rs_matrix}
\title{Shiller's repeat-sales matrices}

\description{
Create a function to compute the \eqn{Z}{Z}, \eqn{X}{X}, \eqn{y}{y}, and \eqn{Y}{Y} matrices in Shiller (1991, sections I-II) from sales-pair data in order to calculate a repeat-sales price index.
}

\usage{
rs_matrix(t2, t1, p2, p1, f = NULL, sparse = FALSE)
}

\arguments{
\item{t2, t1}{A pair of vectors giving the time period of the second and first sale, respectively. Usually a vector of dates, but other values are possible if they can be coerced to character and sorted in chronological order (i.e., with \code{\link[=order]{order()}}).}

\item{p2, p1}{A pair of numeric vectors giving the price of the second and first sale, respectively.}

\item{f}{An optional factor the same length as \code{t1} and \code{t2}, or a vector to be turned into a factor, that is used to group sales.}

\item{sparse}{Should sparse matrices from the \pkg{Matrix} package be used (faster for large datasets), or regular dense matrices (the default)?}
}

\details{
The function returned by \code{rs_matrix()} computes a generalization of the matrices in Shiller (1991, sections I-II) that are applicable to grouped data. These are useful for calculating separate indexes for many, say, cities without needing an explicit loop.

The \eqn{Z}{Z}, \eqn{X}{X}, and \eqn{Y}{Y} matrices are not well defined if either \code{t1} or \code{t2} have missing values, and an error is thrown in this case. Similarly, it should always be the case that \code{t2 > t1}, otherwise a warning is given.
}

\value{
A function that takes a single argument naming the desired matrix. It returns one of two matrices (\eqn{Z}{Z} and \eqn{X}{X}) or two vectors (\eqn{y}{y} and \eqn{Y}{Y}), either regular matrices if \code{sparse = FALSE}, or sparse matrices of class \code{dgCMatrix} if \code{sparse = TRUE}.
}

\references{
Bailey, M. J., Muth, R. F., and Nourse, H. O. (1963). A regression method for real estate price index construction. \emph{Journal of the American Statistical Association }, 53(304):933-942.

Shiller, R. J. (1991). Arithmetic repeat sales price estimators. \emph{Journal of Housing Economics}, 1(1):110-126.
}

\seealso{
\code{\link{rs_pairs}} for turning sales data into sales pairs.
}

\examples{
# Make some data
x <- data.frame(date = c(3, 2, 3, 2, 3, 3), 
                date_prev = c(1, 1, 2, 1, 2, 1), 
                price = 6:1, 
                price_prev = 1)

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

# Standard errors
vcov <- rs_var(y - Z \%*\% b, Z)
sqrt(diag(vcov)) * grs # delta method

# Calculate the ARS index in Shiller (1991)
b <- solve(crossprod(Z, X), crossprod(Z, Y))[, 1]
# or b <- qr.coef(qr(crossprod(Z, X)), crossprod(Z, Y))
(ars <- 100 / b)

# Standard errors
vcov <- rs_var(Y - X \%*\% b, Z, X)
sqrt(diag(vcov)) * ars^2 / 100 # delta method

# Works with grouped data
x <- data.frame(date = c(3, 2, 3, 2), 
                date_prev = c(2, 1, 2, 1), 
                price = 4:1, 
                price_prev = 1,
                group = c("a", "a", "b", "b"))
                
mat <- with(x, rs_matrix(date, date_prev, price, price_prev, group))
b <- solve(crossprod(mat("Z"), mat("X")), crossprod(mat("Z"), mat("Y")))[, 1]
100 / b
}
