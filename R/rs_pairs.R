rs_pairs <- function(period, product) {
  if ((n <- length(period)) != length(product)) {
    stop(gettext("'period' and 'product' must be the same length"))
  }
  if (n == 0L) return(integer(0L))
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
