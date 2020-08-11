library(rsmatrix)

set.seed(4321)
t2 <- sample(101:200)
t1 <- sample(1:100) 
p2 <- runif(100)
p1 <- runif(100)

x <- data.frame(date = c(3, 2, 3, 2, 3, 3), 
                date_prev = c(1, 1, 2, 1, 2, 1), 
                price = 6:1, 
                price_prev = c(1, 1, 5, 1, 3, 1),
                id = c("a", "b", "b", "c", "c", "d"))
mat <- with(x, rs_matrix(date, date_prev, price, price_prev))
b <- solve(crossprod(mat("Z")), crossprod(mat("Z"), mat("y")))
g <- solve(crossprod(mat("Z"), mat("X")), crossprod(mat("Z"), mat("Y")))

#---- Tests for matrices ----
stopifnot(
  exprs = {
    # tests for Z
    identical(rs_z_(integer(0), character(0)), 
              matrix(double(0), ncol = 0))
    identical(rs_z_(integer(0), character(0), logical(0)), 
              matrix(double(0), ncol = 0))
    identical(rs_z_(rep("a", 2), rep("a", 2)), 
              matrix(0, ncol = 1, nrow = 2, dimnames = list(1:2, "a")))
    identical(rs_z_(c(a = rep("a", 2)), c(b = rep("a", 2)), 1:2), 
              matrix(rep(0, 4), ncol = 2, dimnames = list(c("a1", "a2"), c("1.a", "2.a"))))
    identical(rs_z_(c(a = 2:1), 2:1), 
              matrix(c(0, 0, 0, 0), ncol = 2, dimnames = list(c("a1", "a2"), 1:2)))
    identical(rs_z_(1:2, c(a = 2:1)), 
              matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(c("a1", "a2"), 1:2)))
    identical(rs_z_(3:2, 2:1), 
              matrix(c(0, -1, -1, 1, 1, 0), ncol = 3, dimnames = list(1:2, 1:3)))
    identical(rs_z_(c(a = 2, b = 2), c(1, 1), c("a", "b")),
              matrix(c(-1, 0, 0, -1, 1, 0, 0, 1), ncol = 4, dimnames = list(c("a", "b"), c("a.1", "b.1", "a.2", "b.2"))))
    identical(rs_z_(factor(c(3:2, 2)), c(2:1, 1), letters[c(1, 1, 2)]),
              matrix(c(0, -1, 0, 0, 0, -1, -1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0), ncol = 6, dimnames = list(1:3, c("a.1", "b.1", "a.2", "b.2", "a.3", "b.3"))))
    identical(rs_z_(factor(3:2), 2:1), 
              rs_z_(3:2, 2:1))
    identical(rs_z_(factor(2:1, levels = 1:3), factor(c(a = 1, b = 1))),
              matrix(c(-1, 0, 1, 0), ncol = 2, dimnames = list(c("a", "b"), 1:2)))
    identical(rs_z_(factor(letters[3:2]), factor(letters[2:1])), 
              rs_z_(letters[3:2], letters[2:1]))
    identical(rs_z_(as.Date(c("2017-02-01", "2017-03-01", "2017-01-01")), as.Date(c("2017-01-01", "2017-02-01", "2017-01-01"))), 
              matrix(c(-1, 0, 0, 1, -1, 0, 0, 1, 0), ncol = 3, dimnames = list(1:3, c("2017-01-01", "2017-02-01", "2017-03-01"))))
    all(rowSums(rs_z_(t2, t1)) == 0)
    all(rowSums(abs(rs_z_(t2, t1))) == 2)
    # tests for other matrices
    identical(rs_matrix(integer(0), character(0), integer(0), double(0))("X"), 
              matrix(double(0), ncol = 0))
    identical(rs_matrix(integer(0), character(0), integer(0), double(0))("Y"), double(0))
    identical(rs_matrix(c(2, 4), 1:2, c(2, 5), 1:2)("X"), 
              matrix(c(2, -2, 0, 5), ncol = 2, dimnames = list(1:2, c(2, 4))))
    identical(rs_matrix(c(2, 4), 1:2, c(2, 5), 1:2)("Z"), 
              matrix(c(1, -1, 0, 1), ncol = 2, dimnames = list(1:2, c(2, 4))))
    identical(rs_matrix(c(2, 4), 1:2, c(2, 5), 1:2)("Y"), 
              c("1" = 1, "2" = 0))
    # tests for sparse
    identical(rs_z_(integer(0), integer(0), sparse = TRUE),
              as(matrix(double(0), ncol = 0), "dgCMatrix"))
    identical(rs_z_(1, 1, sparse = TRUE),
              as(matrix(0, ncol = 1, dimnames = list(1, 1)), "dgCMatrix"))
    identical(rs_z_(c(a = "a"), "a", sparse = TRUE),
              as(matrix(0, ncol = 1, dimnames = list("a", "a")), "dgCMatrix"))
    identical(rs_z_(c(2, 2), c(1, 1), c("a", "b"), TRUE),
              as(matrix(c(-1, 0, 0, -1, 1, 0, 0, 1), ncol = 4, dimnames = list(1:2, c("a.1", "b.1", "a.2", "b.2"))), "dgCMatrix"))
    identical(rs_z_(t2, t1, sparse = TRUE), 
              Matrix::Matrix(rs_z_(t2, t1), sparse = TRUE))
    identical(rs_matrix(integer(0), integer(0), integer(0), integer(0), sparse = TRUE)("X"),
              as(matrix(double(0), ncol = 0), "dgCMatrix"))
    identical(rs_matrix(t2, t1, p2, p1, sparse = TRUE)("X"), 
              Matrix::Matrix(rs_matrix(t2, t1, p2, p1)("X"), sparse = TRUE))
    identical(rs_matrix(integer(0), integer(0), integer(0), integer(0), sparse = TRUE)("Y"),
              double(0))
    identical(rs_matrix(c(2, 4), 1:2, c(2, 5), 1:2, sparse = TRUE)("Y"), 
              c("1" = 1, "2" = 0))
    # test results
    # results from lm
    max(abs(b[, 1] - c(1.306078088, 0.943826747))) < .Machine$double.eps^0.5
    # results from vcovHC
    max(abs(rs_var(mat("y") - mat("Z") %*% b, mat("Z")) -
              matrix(c(0.090470592, 0.144521572, 0.144521572, 0.274811790), ncol = 2))) < .Machine$double.eps^0.5
    # results from plm
    max(abs(rs_var(mat("y") - mat("Z") %*% b, mat("Z"), ids = x$id) -
              matrix(c(0.091047862, 0.162948279, 0.162948279, 0.310083942), ncol = 2))) < .Machine$double.eps^0.5
    # results from ivreg
    max(abs(g[, 1] - c(0.2375, 0.3000))) < .Machine$double.eps^0.5
    # results from vcovHC
    max(abs(rs_var(mat("Y") - mat("X") %*% g, mat("Z"), mat("X")) - 
              matrix(c(0.003587000, 0.007032129, 0.007032129, 0.017439844), ncol = 2))) < .Machine$double.eps^0.5
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
