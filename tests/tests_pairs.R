library(rsmatrix)

#---- Tests for finding previous values ----
# internal function to find previous value for an integer vector
prev <- function(x) {
  ord <- order(x, na.last = NA)
  res <- rep.int(NA_integer_, length(x))
  res[ord] <- ord[c(1L, seq_len(length(ord) - 1L))]
  res
}

# previous method
rs_pairs2 <- function(period, product) {
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

# An easy-to-verify test
# y x  x_prev pos
# b 1  1      1
# c 10 7      10
# a 1  1      3
# a 2  1      3
# a 3  2      4
# b 3  2      7
# b 2  1      1
# d 1  1      8
# c 11 10     2
# c 7  7      10
# d 1  1      8

x <- c(1, 10, 1:3, 3:2, 1, 11, 7, 1)
y <- letters[c(2, 3, 1, 1, 1, 2, 2, 4, 3, 3, 4)]
rs_pairs(x, y)

x[2] <- NA
rs_pairs(x, y)

y[5] <- NA
rs_pairs(x, y)

# Should return an integer
typeof(rs_pairs(x, y))

rs_pairs(numeric(0), character(0))

# Should return 1:10
rs_pairs(rep(1, 10), 1:10)

# Should return c(1, 1:9)
rs_pairs(1:10, rep(1, 10))

# Should return 1
rs_pairs(1, 1)

# Test against an older implementation
set.seed(4321)

all(replicate(100, {
  x <- as.character(replace(sample(1e3), sample(1e3, 25), NA))
  y <- replace(sample(letters, 1e3, TRUE), sample(1e3, 100), NA)
  all.equal(rs_pairs(x, y), rs_pairs2(x, y))
}))
