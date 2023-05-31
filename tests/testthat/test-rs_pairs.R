test_that("an easy example works", {
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
  y <- factor(letters[c(2, 3, 1, 1, 1, 2, 2, 4, 3, 3, 4)])

  expect_identical(
    rs_pairs(x, y),
    c(1L, 10L, 3L, 3L, 4L, 7L, 1L, 8L, 2L, 10L, 8L)
  )

  x[2] <- NA
  expect_identical(
    rs_pairs(x, y),
    c(1L, NA, 3L, 3L, 4L, 7L, 1L, 8L, 10L, 10L, 8L)
  )

  y[5] <- NA
  expect_identical(
    rs_pairs(x, y),
    c(1L, NA, 3L, 3L, NA, 7L, 1L, 8L, 10L, 10L, 8L)
  )

  x[7] <- NA
  expect_identical(
    rs_pairs(x, y),
    c(1L, NA, 3L, 3L, NA, 1L, NA, 8L, 10L, 10L, 8L)
  )
})

test_that("a more complex example works", {
  x <- c(
    "3", "15", "9", "1", NA, "8", NA, "7", NA, NA, "5", NA, "13", "14", "11"
  )
  y <- c("c", "b", NA, "b", "b", "b", "a", "b", NA, "c", "b", "c", "b", "b", NA)
  expect_identical(
    rs_pairs(x, y),
    c(1L, 14L, NA, 4L, NA, 8L, NA, 11L, NA, NA, 2L, NA, 4L, 13L, NA)
  )
})

test_that("corner cases work", {
  expect_identical(rs_pairs(numeric(0), character(0)), integer(0))
  expect_identical(rs_pairs(1, 1), 1L)
  expect_identical(rs_pairs(rep(1, 10), 1:10), 1:10)
  expect_identical(rs_pairs(1:10, rep(1, 10)), c(1L, 1:9))
  expect_identical(rs_pairs(c(1, 2, 3, 2), rep(1, 4)), c(1L, 1L, 4L, 2L))
})

test_that("different length inputs is an error", {
  expect_error(rs_pairs(1:3, 1:2))
})
