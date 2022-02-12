rs_pairs <- function(period, product) {
  if (length(period) != length(product)) {
    stop(gettext("'period' and 'product' must be the same length"))
  }
  if (!length(period)) return(integer(0L))
  period <- as.factor(period)
  product <- as.factor(product)
  # faster to use numeric codes
  attributes(period) <- NULL
  attributes(product) <- NULL
  ord <- order(product, period, na.last = NA)
  res <- rep.int(NA_integer_, length(period))
  # offset the period by product ordering
  res[ord] <- ord[c(1L, seq_len(length(ord) - 1L))]
  # the first period for each product points to the last period
  # for the previous product
  first <- which(product[res] != product)
  res[first] <- first
  res
}
