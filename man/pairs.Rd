\name{rs_pairs}
\alias{rs_pairs}

\title{Sales pairs}

\description{
Turn repeat sales data into sales pairs that are suitable for making repeat-sales matrices.
}

\usage{
rs_pair(period, product)
}

\arguments{
\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each transaction. The ordering of time periods follows the levels of period to agree with \code{\link[=cut.Date]{cut()}}}.

\item{product}{A factor, or something that can be coerced into one, that gives the corresponding product identifier for each transaction.}
}

\value{
A numeric vector of indices for the previous transaction, with the convention that the previous transaction for the first transaction is itself.
}

\seealso{
\code{\link{rs_matrix}} for using sales pairs to make a repeat-sales index.
}

\examples{
# Make sales pairs

x <- data.frame(id = c(1, 1, 1, 2, 2, 3, 3, 3), 
                date = c(1, 2, 3, 1, 3, 4, 1, 2), 
                price = c(1, 3, 2, 1, 1, 1, 2, 3))
                
pairs <- rs_pairs(x$date, x$id)

x[c("date_prev", "price_prev")] <- x[c("date", "price")][pairs, ]

x
}