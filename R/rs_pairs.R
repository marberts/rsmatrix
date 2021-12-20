# internal function to find previous value for an integer vector
prev <- function(x) {
  len <- length(x)
  if (!len) return(integer(0L))
  ord <- order(x)
  res <- integer(len)
  res[ord] <- ord[c(1L, seq_len(len - 1L))]
  if (anyNA(x)) res[is.na(x)] <- NA
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