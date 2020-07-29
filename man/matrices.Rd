\name{matrices}
\alias{rs_z}
\alias{rs_x}
\title{Shiller's Z and X matrices}

\description{
Compute the Z and X matrices in Shiller (1991, sections I-II) from sales-pair data to calculate a repeat-sales price index.
}

\usage{
rs_z(t2, t1, f, sparse = FALSE)

rs_x(p2, p1, z)
}

\arguments{
\item{t2, t1}{A pair of vectors giving the time period of the second and first sale, respectively. Usually a vector of dates.}

\item{f}{An optional factor the same length as \code{t1} and \code{t2}, or a vector to be turned into a factor, that is used to group \code{t2} and \code{t1}.}

\item{sparse}{Should sparse matrices from 'Matrix' be used in the calculation (faster for large datasets), or regular dense matrices?}

\item{p2, p1}{A pair of numeric vectors giving the price of the second and first sale, respectively.}

\item{z}{Shiller's Z matrix.}
}

\value{
A matrix.
}

\details{
The Z matrix is not well defined if either \code{t1} or \code{t2} have missing values, and an error is thrown if any inputs have missing values. Similarly, it should always be the case that \code{t2 > t1}, otherwise a warning is given.
}

\references{
Shiller, R. J. (1991). Arithmetic repeat sales price estimators. \emph{Journal of Housing Economics}, 1(1):110-126.
}

\examples{
# Make some data
x <- data.frame(date = c(3, 2, 3), 
                date_prev = c(1, 1, 2), 
                price = 3:1, 
                price_prev = 1)

# Calculate the GRS index
Z <- rs_z(x$date, x$date_prev)
Z1 <- Z[, -1] # set period 1 as ref period
y <- log(x$price) - log(x$price_prev)
b <- solve(crossprod(Z1), crossprod(Z1, y))[, 1]
exp(b) * 100

# Calculate the ARS index
X <- rs_x(x$price, x$price_prev, Z)
X1 <- X[, -1] # set period 1 as ref period
Y <- -X[, 1]
b <- solve(crossprod(Z1, X1), crossprod(Z1, Y))[, 1]
100 / b

# Works with grouped data
x <- data.frame(date = c(3, 2, 3, 2), 
                date_prev = c(2, 1, 2, 1), 
                price = 4:1, 
                price_prev = 1,
                group = c("a", "a", "b", "b"))
                
Z <- rs_z(x$date, x$date_prev, x$group)
Z1 <- Z[, -c(1, 4)] # set period 1 as ref period for each group
X <- rs_x(x$price, x$price_prev, Z)
X1 <- X[, -c(1, 4)] # set period 1 as ref period fo each group
Y <- -rowSums(X[, c(1, 4)]) # make sure Y is a vector
b <- solve(crossprod(Z1, X1), crossprod(Z1, Y))[, 1]
100 / b
}