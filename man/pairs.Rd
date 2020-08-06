\name{sales pairs}
\alias{rs_pair}
\alias{rs_unpair}

\title{Make/unmake sales pairs}

\description{
Turn a long dataset with repeat sales information into a dataset of sales pairs, or turn a wide dataset of sales pairs into a long data set with a single price and date variable.
}

\usage{
rs_pair(
  x, 
  id = "id", 
  date = "date", 
  price = "price"
)

rs_unpair(
  x,
  id = "id",
  date = "date",
  date_prev = "date_prev",
  price = "price",
  price_prev = "price_prev"
)

}
\arguments{
\item{x}{A data.frame, or something that can be coerced into one.}

\item{id}{Column name in \code{x} for a unique id.}

\item{date}{Column name in \code{x} for a sales date.}

\item{price}{Column name in \code{x} for a sales price.}

\item{date_prev}{Column name in \code{x} for a previous sales date.}

\item{price_prev}{Column name in \code{x} for a previous sales price.}
}

\value{
A data.frame.
}

\details{
Making sales pairs takes consecutive sales for each \code{id} and turns them into pairs of consecutive sales. This is equivalent to first-differencing the data.

Observations with missing values and duplicated observations are not removed prior to creating sales pairs. Making sales pairs is generally not well defined with missing and duplicated values, and these should be removed prior to making pairs.

Extra columns with data that do not varying over time for each id are preserved when making pairs.

When unmaking sales pairs, the previous price and date for the earliest sale are extracted for each id, then concatenated (row-wise) back into \code{x}. Extra columns are expanded in the long format.

As with making sales pairs, un-making sales pairs is generally not well defined with missing and duplicated values, and these should be removed prior to un-making pairs.
}

\examples{
# Turn into a 'wide' dataset of sales pairs
x <- data.frame(
id = c(1, 1, 1, 2, 2), 
date = c(1, 2, 3, 1, 3), 
price = c(1, 3, 2, 1, 1)
)
rs_pair(x)

# Turn into a 'long' dataset of sales pairs
x <- data.frame(
id = c(1, 1, 2), 
date = c(2, 3, 3), 
date_prev = c(1, 2, 1), 
price = 3:1, 
price_prev = c(1, 3, 1)
)
rs_unpair(x)
}