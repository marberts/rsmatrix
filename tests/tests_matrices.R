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
                id = c("a", "b", "b", "c", "c", "d"),
                id2 = rep(c("a", "b"), each = 3))

mat <- with(x, rs_matrix(date, date_prev, price, price_prev))
mats <- with(x, rs_matrix(date, date_prev, price, price_prev, sparse = TRUE))
matg <- with(x, rs_matrix(date, date_prev, price, price_prev, id2))
mata <- with(subset(x, id2 == "a"),
             rs_matrix(date, date_prev, price, price_prev))

b <- solve(crossprod(mat("Z")), crossprod(mat("Z"), mat("y")))
bg <- solve(crossprod(matg("Z")), crossprod(matg("Z"), matg("y")))
ba <- solve(crossprod(mata("Z")), crossprod(mata("Z"), mata("y")))

g <- solve(crossprod(mat("Z"), mat("X")), crossprod(mat("Z"), mat("Y")))
gg <- solve(crossprod(matg("Z"), matg("X")), crossprod(matg("Z"), matg("Y")))
ga <- solve(crossprod(mata("Z"), mata("X")), crossprod(mata("Z"), mata("Y")))

#---- Tests for matrices ----
stopifnot(
  exprs = {
    # tests for Z
    identical(.rs_z(integer(0), character(0)), 
              matrix(double(0), ncol = 0))
    identical(.rs_z(integer(0), character(0), logical(0)), 
              matrix(double(0), ncol = 0))
    identical(.rs_z(rep("a", 2), rep("a", 2)), 
              matrix(0, ncol = 1, nrow = 2, dimnames = list(1:2, "a")))
    identical(.rs_z(c(a = rep("a", 2)), c(b = rep("a", 2)), 1:2), 
              matrix(rep(0, 4), ncol = 2, dimnames = list(c("a1", "a2"), c("1.a", "2.a"))))
    identical(.rs_z(c(a = 2:1), 2:1), 
              matrix(c(0, 0, 0, 0), ncol = 2, dimnames = list(c("a1", "a2"), 1:2)))
    identical(.rs_z(1:2, c(a = 2:1)), 
              matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(c("a1", "a2"), 1:2)))
    identical(.rs_z(3:2, 2:1), 
              matrix(c(0, -1, -1, 1, 1, 0), ncol = 3, dimnames = list(1:2, 1:3)))
    identical(.rs_z(c(a = 2, b = 2), c(1, 1), c("a", "b")),
              matrix(c(-1, 0, 0, -1, 1, 0, 0, 1), ncol = 4, dimnames = list(c("a", "b"), c("a.1", "b.1", "a.2", "b.2"))))
    identical(.rs_z(factor(c(3:2, 2)), c(2:1, 1), letters[c(1, 1, 2)]),
              matrix(c(0, -1, 0, 0, 0, -1, -1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0), ncol = 6, dimnames = list(1:3, c("a.1", "b.1", "a.2", "b.2", "a.3", "b.3"))))
    identical(.rs_z(factor(3:2), 2:1), 
              .rs_z(3:2, 2:1))
    identical(.rs_z(factor(2:1, levels = 1:3), factor(c(a = 1, b = 1))),
              matrix(c(-1, 0, 1, 0), ncol = 2, dimnames = list(c("a", "b"), 1:2)))
    identical(.rs_z(factor(letters[3:2]), factor(letters[2:1])), 
              .rs_z(letters[3:2], letters[2:1]))
    identical(.rs_z(as.Date(c("2017-02-01", "2017-03-01", "2017-01-01")), as.Date(c("2017-01-01", "2017-02-01", "2017-01-01"))), 
              matrix(c(-1, 0, 0, 1, -1, 0, 0, 1, 0), ncol = 3, dimnames = list(1:3, c("2017-01-01", "2017-02-01", "2017-03-01"))))
    all(rowSums(.rs_z(t2, t1)) == 0)
    all(rowSums(abs(.rs_z(t2, t1))) == 2)
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
    identical(.rs_z(integer(0), integer(0), sparse = TRUE),
              as(matrix(double(0), ncol = 0), "dgCMatrix"))
    identical(.rs_z(1, 1, sparse = TRUE),
              as(matrix(0, ncol = 1, dimnames = list(1, 1)), "dgCMatrix"))
    identical(.rs_z(c(a = "a"), "a", sparse = TRUE),
              as(matrix(0, ncol = 1, dimnames = list("a", "a")), "dgCMatrix"))
    identical(.rs_z(c(2, 2), c(1, 1), c("a", "b"), TRUE),
              as(matrix(c(-1, 0, 0, -1, 1, 0, 0, 1), ncol = 4, dimnames = list(1:2, c("a.1", "b.1", "a.2", "b.2"))), "dgCMatrix"))
    identical(.rs_z(t2, t1, sparse = TRUE), 
              Matrix::Matrix(.rs_z(t2, t1), sparse = TRUE))
    identical(rs_matrix(integer(0), integer(0), integer(0), integer(0), sparse = TRUE)("X"),
              as(matrix(double(0), ncol = 0), "dgCMatrix"))
    identical(rs_matrix(t2, t1, p2, p1, sparse = TRUE)("X"), 
              Matrix::Matrix(rs_matrix(t2, t1, p2, p1)("X"), sparse = TRUE))
    identical(rs_matrix(integer(0), integer(0), integer(0), integer(0), sparse = TRUE)("Y"),
              double(0))
    identical(rs_matrix(c(2, 4), 1:2, c(2, 5), 1:2, sparse = TRUE)("Y"), 
              c("1" = 1, "2" = 0))
    # test results
    identical(as.numeric(ba[, 1]), as.numeric(bg[seq(1, 4, 2), 1]))
    identical(as.numeric(ga[, 1]), as.numeric(gg[seq(1, 4, 2), 1]))
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
    max(abs(rs_var(mat("Y") - mat("X") %*% g, mat("Z"), mat("X")) - 
              rs_var(mats("Y") - mats("X") %*% g, mats("Z"), mats("X")))) < .Machine$double.eps^0.5
  }, 
  local = getNamespace("rsmatrix")
)