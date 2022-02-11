# internal function to find previous value for an integer vector
prev <- function(x) {
  ord <- order(x, na.last = NA)
  res <- rep.int(NA_integer_, length(x))
  res[ord] <- ord[c(1L, seq_len(length(ord) - 1L))]
  res
}

rs_pairs <- function(period, product) {
  if (length(period) != length(product)) {
    stop(gettext("'period' and 'product' must be the same length"))
  }
  if (!length(period)) return(integer(0L))
  period <- as.factor(period)
  attributes(period) <- NULL # faster to use numeric codes
  product <- as.factor(product)
  res <- split(seq_along(period), product)
  period <- split(period, product)
  if (max(vapply(period, anyDuplicated, numeric(1L), incomparables = NA))) {
    warning(gettext("there are duplicated period-product pairs"))
  }
  m <- lapply(period, prev)
  res <- .mapply(`[`, list(res, m), list())
  unsplit(res, product)
}