#---- Tests for pair/unpair ----
x <- data.frame(id = c(1, 2), 
                date = c(3, 2), 
                price = c(4, 2), 
                x = c(2, 1), 
                date_prev = c(1, 1), 
                price_prev = c(3, 1))
y <- data.frame(id = c(2, 2, 1, 1, 3), 
                date = c(1, 2, 1, 3, 2), 
                price = c(1, 2, 3, 4, 5), 
                x = c(1, 1, 2, 2, 1))
stopifnot(
  exprs = {
    identical(rs_pair(y), x)
    identical(rs_unpair(x), 
              as.data.frame(y[order(y$id), ][-5, ], row.names = 1:4))
    identical(rs_unpair(rs_pair(data.frame(id = character(0), date = character(0), price = character(0)))), 
              data.frame(id = character(0), date = character(0), price = character(0))) 
  },
  local = getNamespace("rsmatrix")
)