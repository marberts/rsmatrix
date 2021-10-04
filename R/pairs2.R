offset <- function(x) {
  dr <- match(x, sort(unique(x)))
  match(pmax(dr - 1L, 1L), x, incomparables = NA)
}

rs_pairs <- function(period, product) {
  if (length(period) != length(product)) {
    stop(gettext("'period' and 'product' must be the same length"))
  }
  period <- as.factor(period)
  attributes(period) <- NULL
  if (!length(period)) return(period[0])
  product <- as.factor(product)
  res <- split(seq_along(period), product)
  period <- split(period, product)
  if (max(vapply(period, anyDuplicated, numeric(1)))) {
    warning(gettext("there are duplicated period-product pairs"))
  }
  m <- lapply(period, offset)
  res <- .mapply(`[`, list(res, m), list())
  unlist(res, use.names = FALSE)
}
