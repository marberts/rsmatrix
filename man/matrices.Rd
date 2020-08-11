\name{rs_matrix}
\alias{rs_matrix}
\title{Shiller's repeat-sales matrices}

\description{
Create a function to compute the Z, X, y, and Y matrices in Shiller (1991, sections I-II) from sales-pair data to calculate a repeat-sales price index.
}

\usage{
rs_matrix(t2, t1, p2, p1, f = NULL, sparse = FALSE)
}

\arguments{
\item{t2, t1}{A pair of vectors giving the time period of the second and first sale, respectively. Usually a vector of dates.}

\item{p2, p1}{A pair of numeric vectors giving the price of the second and first sale, respectively.}

\item{f}{An optional factor the same length as \code{t1} and \code{t2}, or a vector to be turned into a factor, that is used to group \code{t2} and \code{t1}.}

\item{sparse}{Should sparse matrices from 'Matrix' be used in the calculation (faster for large datasets), or regular dense matrices?}
}

\value{
A function that takes a single argument naming the desired matrix. It returns one of two matrices (Z and X) or two vectors (y and Y), either regular matrices if \code{sparse = FALSE}, or a sparse matrices of class 'dgCMatrix' if \code{sparse = TRUE}.
}

\details{
The function returned by \code{rs_matrix()} computes a generalization of the matrices in Shiller (1991, sections I-II) that are applicable to grouped data. This is useful for calculating separate indexes for many, say, cities without needing an explicit loop.

The Z, X, and Y matrices are not well defined if either \code{t1} or \code{t2} have missing values, and an error is thrown if any inputs have missing values. Similarly, it should always be the case that \code{t2 > t1}, otherwise a warning is given.
}

\references{
Shiller, R. J. (1991). Arithmetic repeat sales price estimators. \emph{Journal of Housing Economics}, 1(1):110-126.
}

\examples{
# Make some data
x <- data.frame(date = c(3, 2, 3, 2, 3, 3), 
                date_prev = c(1, 1, 2, 1, 2, 1), 
                price = 6:1, 
                price_prev = 1)

# Calculate matrices
mat <- with(x, rs_matrix(date, date_prev, price, price_prev))
mat("Z") # Z matrix
mat("X") # X matrix
mat("y") # y vector
mat("Y") # Y vector

# Calculate the GRS index in Bailey, Muth, and Nourse (1963)
b <- solve(crossprod(mat("Z")), crossprod(mat("Z"), mat("y")))[, 1]
# or b <- qr.coef(qr(mat("Z")), mat("y"))
exp(b) * 100

# Calculate the ARS index
b <- solve(crossprod(mat("Z"), mat("X")), crossprod(mat("Z"), mat("Y")))[, 1]
# or b <- qr.coef(qr(crossprod(mat("Z"), mat("X"))), crossprod(mat("Z"), mat("Y")))
100 / b

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