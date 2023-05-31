# data for computations
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
matb <- with(subset(x, id2 == "b"),
             rs_matrix(date, date_prev, price, price_prev))

b <- solve(crossprod(mat("Z")), crossprod(mat("Z"), mat("y")))
bg <- solve(crossprod(matg("Z")), crossprod(matg("Z"), matg("y")))
ba <- solve(crossprod(mata("Z")), crossprod(mata("Z"), mata("y")))
bb <- solve(crossprod(matb("Z")), crossprod(matb("Z"), matb("y")))

g <- solve(crossprod(mat("Z"), mat("X")), crossprod(mat("Z"), mat("Y")))
gg <- solve(crossprod(matg("Z"), matg("X")), crossprod(matg("Z"), matg("Y")))
ga <- solve(crossprod(mata("Z"), mata("X")), crossprod(mata("Z"), mata("Y")))
gb <- solve(crossprod(matb("Z"), matb("X")), crossprod(matb("Z"), matb("Y")))

test_that("corner cases work", {
  m <- rs_matrix(
    integer(0),
    character(0),
    integer(0),
    double(0),
    factor(integer(0), letters)
  )
  expect_identical(m("Z"), matrix(double(0), ncol = 0))
  expect_identical(m("X"), matrix(double(0), ncol = 0))
  expect_identical(m("Y"), double(0))
  expect_identical(m("y"), double(0))

  ms <- rs_matrix(
    integer(0),
    character(0),
    integer(0),
    double(0),
    factor(integer(0), letters),
    sparse = TRUE
  )
  expect_identical(
    ms("Z"),
    as(as(matrix(double(0), ncol = 0), "generalMatrix"), "CsparseMatrix")
  )
  expect_identical(
    ms("X"),
    as(as(matrix(double(0), ncol = 0), "generalMatrix"), "CsparseMatrix")
  )
  expect_identical(ms("Y"), double(0))
  expect_identical(ms("y"), double(0))
})

test_that("matrices are correct for a simple case", {
  m <- rs_matrix(c(2, 4), 1:2, c(2, 5), 1:2)
  expect_identical(
    m("X"),
    matrix(c(2, -2, 0, 5), ncol = 2, dimnames = list(1:2, c(2, 4)))
  )
  expect_identical(
    m("Z"),
    matrix(c(1, -1, 0, 1), ncol = 2, dimnames = list(1:2, c(2, 4)))
  )
  expect_identical(m("Y"), c("1" = 1, "2" = 0))
  expect_identical(m("y"), c("1" = log(2), "2" = log(5 / 2)))

  ms <- rs_matrix(c(2, 4), 1:2, c(2, 5), 1:2, sparse = TRUE)
  expect_identical(as.matrix(ms("X")), m("X"))
  expect_identical(as.matrix(ms("Z")), m("Z"))
  expect_identical(ms("Y"), c("1" = 1, "2" = 0))
  expect_identical(ms("y"), c("1" = log(2), "2" = log(5 / 2)))
})

test_that("Z matrix works correctly", {
  expect_identical(rsmatrix:::rs_z_(integer(0), character(0)),
                   matrix(numeric(0), ncol = 0))
  expect_identical(rsmatrix:::rs_z_(integer(0), character(0), logical(0)),
                   matrix(numeric(0), ncol = 0))
  expect_identical(
    suppressWarnings(rsmatrix:::rs_z_(rep("a", 2), rep("a", 2))),
    matrix(0, ncol = 1, nrow = 2, dimnames = list(1:2, "a"))
  )
  expect_identical(
    suppressWarnings(
      rsmatrix:::rs_z_(c(a = rep("a", 2)), c(b = rep("a", 2)), 1:2)
    ),
    matrix(rep(0, 4), ncol = 2, dimnames = list(c("a1", "a2"), c("1.a", "2.a")))
  )
  expect_identical(
    suppressWarnings(rsmatrix:::rs_z_(c(a = 2:1), 2:1)),
    matrix(c(0, 0, 0, 0), ncol = 2, dimnames = list(c("a1", "a2"), 1:2))
  )
  expect_identical(
    suppressWarnings(rsmatrix:::rs_z_(1:2, c(a = 2:1))),
    matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(c("a1", "a2"), 1:2))
  )
  expect_identical(
    rsmatrix:::rs_z_(3:2, 2:1),
    matrix(c(0, -1, -1, 1, 1, 0), ncol = 3, dimnames = list(1:2, 1:3))
  )
  expect_identical(
    rsmatrix:::rs_z_(c(a = 2, b = 2), c(1, 1), c("a", "b")),
    matrix(c(-1, 0, 0, -1, 1, 0, 0, 1), ncol = 4,
           dimnames = list(c("a", "b"), c("a.1", "b.1", "a.2", "b.2")))
  )
  expect_identical(
    rsmatrix:::rs_z_(factor(c(3:2, 2)), c(2:1, 1), letters[c(1, 1, 2)]),
    matrix(c(0, -1, 0, 0, 0, -1, -1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0), ncol = 6,
           dimnames = list(1:3, c("a.1", "b.1", "a.2", "b.2", "a.3", "b.3")))
  )
  expect_identical(rsmatrix:::rs_z_(factor(3:2), 2:1),
                   rsmatrix:::rs_z_(3:2, 2:1))
  expect_identical(
    suppressWarnings(
      rsmatrix:::rs_z_(factor(2:1, levels = 1:3), factor(c(a = 1, b = 1)))
    ),
    matrix(c(-1, 0, 1, 0), ncol = 2, dimnames = list(c("a", "b"), 1:2))
  )
  expect_identical(rsmatrix:::rs_z_(factor(letters[3:2]), factor(letters[2:1])),
                   rsmatrix:::rs_z_(letters[3:2], letters[2:1]))
  expect_identical(
    suppressWarnings(
      rsmatrix:::rs_z_(as.Date(c("2017-02-01", "2017-03-01", "2017-01-01")),
                       as.Date(c("2017-01-01", "2017-02-01", "2017-01-01")))
    ),
    matrix(c(-1, 0, 0, 1, -1, 0, 0, 1, 0), ncol = 3,
           dimnames = list(1:3, c("2017-01-01", "2017-02-01", "2017-03-01")))
  )
})

test_that("sparse matrices work correctly", {
  expect_identical(rsmatrix:::rs_z_(integer(0), integer(0), sparse = TRUE),
                   rsmatrix:::dense_to_sparse(matrix(integer(0), ncol = 0)))
  expect_identical(
    suppressWarnings(rsmatrix:::rs_z_(1, 1, sparse = TRUE)),
    rsmatrix:::dense_to_sparse(matrix(0, ncol = 1, dimnames = list(1, 1)))
  )
  expect_identical(
    suppressWarnings(rsmatrix:::rs_z_(c(a = "a"), "a", sparse = TRUE)),
    rsmatrix:::dense_to_sparse(matrix(0, ncol = 1, dimnames = list("a", "a")))
  )
  expect_identical(
    rsmatrix:::rs_z_(c(2, 2), c(1, 1), c("a", "b"), TRUE),
    rsmatrix:::dense_to_sparse(
      matrix(c(-1, 0, 0, -1, 1, 0, 0, 1), ncol = 4,
             dimnames = list(1:2, c("a.1", "b.1", "a.2", "b.2"))))
    )
})

test_that("grouped indexes work", {
  expect_equal(as.numeric(ba[, 1]), as.numeric(bg[seq(1, 4, 2), 1]))
  expect_equal(as.numeric(ga[, 1]), as.numeric(gg[seq(1, 4, 2), 1]))
  expect_equal(as.numeric(bb[, 1]), as.numeric(bg[seq(2, 4, 2), 1]))
  expect_equal(as.numeric(gb[, 1]), as.numeric(gg[seq(2, 4, 2), 1]))
})

test_that("index calculation agrees with regressions", {
  # results from lm
  expect_equal(as.numeric(b), c(1.306078088475809, 0.943826746689325))
  # results from vcovHC
  expect_equal(
    unname(rs_var(mat("y") - mat("Z") %*% b, mat("Z"))),
    matrix(c(0.0904705916756374, 0.1445215722595884,
             0.1445215722595884, 0.2748117902801680),
           ncol = 2)
  )
  # results from plm
  expect_equal(
    unname(rs_var(mat("y") - mat("Z") %*% b, mat("Z"), ids = x$id)),
    matrix(c(0.091047862, 0.162948279, 0.162948279, 0.310083942), ncol = 2)
  )
  # results from ivreg
  expect_equal(as.numeric(g), c(0.2375, 0.3000))
  # results from vcovHC
  expect_equal(
    unname(rs_var(mat("Y") - mat("X") %*% g, mat("Z"), mat("X"))),
    matrix(c(0.00358699951171875, 0.00703212890625000,
             0.00703212890625000, 0.01743984374999999),
           ncol = 2)
  )
  expect_equal(
    as.numeric(rs_var(mat("Y") - mat("X") %*% g, mat("Z"), mat("X"))),
    as.numeric(rs_var(mats("Y") - mats("X") %*% g, mats("Z"), mats("X")))
  )
})

test_that("errors work", {
  expect_error(rs_matrix(1:4, 1:3, 1:4, 1:4))
  expect_error(rs_matrix(1:4, 1:4, 1:4, 1:4, 1:5))
  expect_error(rs_matrix(c(1:3, NA), 1:4, 1:4, 1:4))
  expect_error(rs_matrix(1:4, 1:4, 1:4, 1:4, c(1:3, NA)))
})
