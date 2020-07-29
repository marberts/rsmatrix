set.seed(4321)
t2 <- sample(101:200)
t1 <- sample(1:100) 
p2 <- runif(100)
p1 <- runif(100)

#---- Tests for matrices ----
stopifnot(
  exprs = {
    # tests for Z
    identical(rs_z(2:1, 2:1), 
              matrix(c(0, 0, 0, 0), ncol = 2, dimnames = list(1:2, 1:2)))
    identical(rs_z(1:2, 2:1), 
              matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(1:2, 1:2)))
    identical(rs_z(3:2, 2:1), 
              matrix(c(0, -1, -1, 1, 1, 0), ncol = 3, dimnames = list(1:2, 1:3)))
    identical(rs_z(c(2, 2), c(1, 1), c("a", "b")),
              matrix(c(-1, 0, 1, 0, 0, -1, 0, 1), ncol = 4, dimnames = list(1:2, paste(c("a", "a", "b", "b"), 1:2, sep = ":"))))
    identical(rs_z(factor(c(3:2, 2)), c(2:1, 1), letters[c(1, 1, 2)]),
              matrix(c(0, -1, 0, -1, 1, 0, 1, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0), ncol = 6, dimnames = list(1:3, paste(c("a", "a", "a", "b", "b", "b"), 1:3, sep = ":"))))
    identical(rs_z(factor(3:2), 2:1), 
              rs_z(3:2, 2:1))
    identical(rs_z(factor(letters[3:2]), factor(letters[2:1])), 
              rs_z(letters[3:2], letters[2:1]))
    identical(rs_z(as.Date(c("2017-02-01", "2017-03-01", "2017-01-01")), as.Date(c("2017-01-01", "2017-02-01", "2017-01-01"))), 
              matrix(c(-1, 0, 0, 1, -1, 0, 0, 1, 0), ncol = 3, dimnames = list(1:3, c("2017-01-01", "2017-02-01", "2017-03-01"))))
    all(rowSums(rs_z(t2, t1)) == 0)
    all(rowSums(abs(rs_z(t2, t1))) == 2)
    # tests for X
    identical(rs_x(c(2, 5), 1:2, rs_z(c(2, 4), 1:2)), 
              matrix(c(-1, 0, 2, -2, 0, 5), ncol = 3, dimnames = list(1:2, c(1, 2, 4))))
    all(rowSums(rs_x(p2, p1, rs_z(t2, t1))) == p2 - p1)
    all(rowSums(abs(rs_x(p2, p1, rs_z(t2, t1)))) == p2 + p1)
    # tests for sparse
    identical(rs_z(t2, t1, sparse = TRUE), 
              Matrix::Matrix(rs_z(t2, t1), sparse = TRUE))
    identical(rs_x(p2, p1, rs_z(t2, t1, sparse = TRUE)), 
              Matrix::Matrix(rs_x(p2, p1, rs_z(t2, t1)), sparse = TRUE))
  }, 
  local = getNamespace("rsmatrix")
)

#---- Tests for pair/unpair ----
x <- data.frame(id = c(1, 2), date = c(3, 2), price = c(4, 2), x = c(2, 1), date_prev = c(1, 1), price_prev = c(3, 1))
y <- data.frame(id = c(2, 2, 1, 1, 3), date = c(1, 2, 1, 3, 2), price = c(1, 2, 3, 4, 5), x = c(1, 1, 2, 2, 1))
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
