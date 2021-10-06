prev <- function(x) {
  len <- length(x)
  if (!len) return(integer(0))
  ord <- order(x)
  res <- rep_len(0L, len)
  res[ord] <- ord[c(1L, seq_len(len - 1L))]
  if (anyNA(x)) res[is.na(x)] <- NA
  res
}

rs_pairs <- function(period, product) {
  if (length(period) != length(product)) {
    stop(gettext("'period' and 'product' must be the same length"))
  }
  if (!length(period)) return(integer(0))
  period <- as.factor(period)
  attributes(period) <- NULL
  product <- as.factor(product)
  res <- split(seq_along(period), product)
  period <- split(period, product)
  if (max(vapply(period, anyDuplicated, numeric(1)))) {
    warning(gettext("there are duplicated period-product pairs"))
  }
  m <- lapply(period, prev)
  res <- .mapply(`[`, list(res, m), list())
  unsplit(res, product)
}