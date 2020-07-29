stopifnot(
  exprs = {
    identical(rs_z(1:2, 2:1), matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(1:2, 1:2)))
    identical(rs_z(3:2, 2:1), matrix(c(0, -1, -1, 1, 1, 0), ncol = 3, dimnames = list(1:2, 1:3)))
    identical(rs_z(c(2, 2), c(1, 1), c('a', 'b')), matrix(c(-1, 0, 1, 0, 0, -1, 0, 1), ncol = 4, dimnames = list(1:2, paste(c('a', 'a', 'b', 'b'), 1:2, sep = ':'))))
    all(rowSums(rs_z(sample(101:200), sample(1:100))) == 0)
    all(rowSums(abs(rs_z(sample(101:200), sample(1:100)))) == 2)
  }, 
  local = getNamespace('ppd')
)

x <- data.frame(id = c(1, 2), date = c(3, 2), price = c(4, 2), x = c(2, 1), date_prev = c(1, 1), price_prev = c(3, 1))
y <- data.frame(id = c(2, 2, 1, 1, 3), date = c(1, 2, 1, 3, 2), price = c(1, 2, 3, 4, 5), x = c(1, 1, 2, 2, 1))
stopifnot(
  exprs = {
    identical(rs_pair(y), x)
    identical(rs_unpair(x), as.data.frame(y[order(y$id), ][-5, ], row.names = 1:4))
    identical(rs_unpair(rs_pair(data.frame(id = character(0), date = character(0), price = character(0)))), data.frame(id = character(0), date = character(0), price = character(0))) 
  },
  local = getNamespace('ppd')
)
