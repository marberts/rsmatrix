#---- Make sales pairs ----
rs_pair <- function(x, id = 'id', date = 'date', price = 'price') {
  # check input
  x <- as.data.frame(x)
  id <- match.arg(id, names(x))
  date <- match.arg(date, names(x))
  price <- match.arg(price, names(x))
  # order data
  x <- x[order(x[[id]], x[[date]]), , drop = FALSE]
  # get previous date and price
  x0 <- x[-nrow(x), c(id, date, price), drop = FALSE]
  x1 <- x[-1, , drop = FALSE]
  rs <- x0[[id]] == x1[[id]]
  x0 <- x0[rs, , drop = FALSE]
  x1 <- x1[rs, , drop = FALSE]
  # make wide dataset
  x0 <- x0[c(date, price)]
  names(x0) <- paste(names(x0), 'prev', sep = '_')
  pairs <- cbind(x1, x0)
  # output
  row.names(pairs) <- NULL
  pairs
}

#---- Unmake sales pairs ----
rs_unpair <- function(x, id = 'id', date = 'date', date_prev = 'date_prev', price = 'price', price_prev = 'price_prev') {
  # check input
  x <- as.data.frame(x)
  id <- match.arg(id, names(x))
  date <- match.arg(date, names(x))
  date_prev <- match.arg(date_prev, names(x))
  price <- match.arg(price, names(x))
  price_prev <- match.arg(price_prev, names(x))
  # order data
  x <- x[order(x[[id]], x[[date]]), , drop = FALSE]
  # get previous price and date
  x0 <- x[!duplicated(x[[id]]), !names(x) %in% c(date, price), drop = FALSE]
  names(x0)[names(x0) == date_prev] <- date
  names(x0)[names(x0) == price_prev] <- price
  # make long dataset
  x1 <- x[!names(x) %in% c(date_prev, price_prev)]
  long <- rbind(x1, x0)
  long <- long[order(long[[id]], long[[date]]), ]
  # output
  row.names(long) <- NULL
  long
}
