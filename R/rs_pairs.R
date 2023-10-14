#' Sales pairs
#'
#' Turn repeat-sales data into sales pairs that are suitable for making
#' repeat-sales matrices.
#'
#'
#' @param period A vector that gives the time period for each sale. Usually a
#' date vector, or a factor with the levels in chronological order, but other
#' values are possible if they can be sorted in chronological order (i.e., with
#' [order()]).
#' @param product A vector that gives the product identifier for each sale.
#' Usually a factor or vector of integer codes for each product.
#' @return A numeric vector of indices giving the position of the previous sale
#' for each `product`, with the convention that the previous sale for the
#' first sale is itself. The first position is returned in the case of ties.
#' @note [`order()`] is the workhorse of `rs_pairs()`,
#' so performance can be sensitive to the types of `period` and
#' `product`, and can be slow for large character vectors.
#' @seealso [rs_matrix()] for using sales pairs to make a
#' repeat-sales index.
#' @examples
#' # Make sales pairs
#' x <- data.frame(
#'   id = c(1, 1, 1, 3, 2, 2, 3, 3),
#'   date = c(1, 2, 3, 2, 1, 3, 4, 1),
#'   price = c(1, 3, 2, 3, 1, 1, 1, 2)
#' )
#'
#' pairs <- rs_pairs(x$date, x$id)
#'
#' x[c("date_prev", "price_prev")] <- x[c("date", "price")][pairs, ]
#'
#' x
#'
#' @export rs_pairs
rs_pairs <- function(period, product) {
  n <- length(period)

  # != is slow for factors with many levels, so use the integer codes
  if (is.factor(product)) {
    attributes(product) <- NULL
  }
  if (length(product) != n) {
    stop("'period' and 'product' must be the same length")
  }
  if (n == 0L) {
    return(integer(0L))
  }

  ord <- order(product, period, na.last = NA)
  res <- rep.int(NA_integer_, n)
  # offset the period by product ordering
  res[ord] <- ord[c(1L, seq_len(length(ord) - 1L))]
  # the first period for each product points to the last period
  # for the previous product
  first <- which(product[res] != product)
  res[first] <- first
  res
}
