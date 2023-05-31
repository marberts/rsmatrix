\name{rs_pairs}
\alias{rs_pairs}

\title{Sales pairs}

\description{
Turn repeat-sales data into sales pairs that are suitable for making repeat-sales matrices.
}

\usage{
rs_pairs(period, product)
}

\arguments{
\item{period}{A vector that gives the time period for each sale. Usually a date vector, or a factor with the levels in chronological order, but other values are possible if they can be sorted in chronological order (i.e., with \code{\link[=order]{order()}}).}

\item{product}{A vector that gives the product identifier for each sale. Usually a factor or vector of integer codes for each product.}
}

\value{
A numeric vector of indices giving the position of the previous sale for each \code{product}, with the convention that the previous sale for the first sale is itself. The first position is returned in the case of ties.
}

\note{
\code{\link[=order]{order()}}) is the workhorse of \code{rs_pairs()}, so performance can be sensitive to the types of \code{period} and \code{product}, and can be slow for large character vectors.
}

\seealso{
\code{\link{rs_matrix}} for using sales pairs to make a repeat-sales index.
}

\examples{
# Make sales pairs
x <- data.frame(id = c(1, 1, 1, 3, 2, 2, 3, 3), 
                date = c(1, 2, 3, 2, 1, 3, 4, 1), 
                price = c(1, 3, 2, 3, 1, 1, 1, 2))
                
pairs <- rs_pairs(x$date, x$id)

x[c("date_prev", "price_prev")] <- x[c("date", "price")][pairs, ]

x
}
