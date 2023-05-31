rs_pairs <- function(period, product) {
  n <- length(period)

  # != is slow for factors with many levels
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
