#' Test if inputs have the same length
#' @noRd
different_lengths <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
}

#' Compute the Z matrix (internal)
#' @noRd
rs_z_ <- function(t2, t1, f = NULL, sparse = FALSE) {
  # coerce t2 and t1 into characters prior to taking the union
  # so that both dates and factors are treated the same
  lev2 <- as.character(unique(t2))
  lev1 <- as.character(unique(t1))
  lev <- sort.int(unique(c(lev2, lev1))) # usually faster than base::union()
  t2 <- factor(t2, lev)
  t1 <- factor(t1, lev)
  if (any(unclass(t2) <= unclass(t1))) {
    warning(
      "all elements of 't2' should be greater than the corresponding ",
      "elements in 't1'"
    )
  }

  # make row names before interacting with f
  nm <- if (!is.null(names(t2))) {
    names(t2)
  } else if (!is.null(names(t1))) {
    names(t1)
  } else if (!is.null(names(f))) {
    names(f)
  } else {
    seq_along(t2)
  }

  if (!is.null(f)) {
    f <- as.factor(f)
    t2 <- interaction(f, t2)
    t1 <- interaction(f, t1)
    lev <- levels(t2)
  }

  # calculate Z
  dims <- c(length(nm), length(lev))
  attributes(t2) <- NULL
  attributes(t1) <- NULL
  non_zero <- which(t2 != t1)
  i <- seq_along(t2)[non_zero]
  t2 <- t2[non_zero]
  t1 <- t1[non_zero]
  if (sparse) {
    res <- Matrix::sparseMatrix(rep.int(i, 2), c(t2, t1),
      x = rep(c(1, -1), each = length(i)),
      dims = dims
    )
  } else {
    res <- rep.int(0, prod(dims))
    res[(t2 - 1L) * dims[1L] + i] <- 1
    res[(t1 - 1L) * dims[1L] + i] <- -1
    dim(res) <- dims
  }

  if (length(lev) > 0L) {
    colnames(res) <- lev
    rownames(res) <- nm
  }

  res
}

#' Compute X matrix (internal)
#' @noRd
rs_x_ <- function(z, p2, p1) (z > 0) * p2 - (z < 0) * p1

#' Shiller's repeat-sales matrices
#'
#' Create a function to compute the \eqn{Z}, \eqn{X}, \eqn{y}, and \eqn{Y}
#' matrices in Shiller (1991, sections I-II) from sales-pair data in order to
#' calculate a repeat-sales price index.
#'
#' The function returned by `rs_matrix()` computes a generalization of the
#' matrices in Shiller (1991, sections I-II) that are applicable to grouped
#' data. These are useful for calculating separate indexes for many, say,
#' cities without needing an explicit loop.
#'
#' The \eqn{Z}, \eqn{X}, and \eqn{Y} matrices are not well defined if either
#' `t1` or `t2` have missing values, and an error is thrown in this
#' case. Similarly, it should always be the case that `t2 > t1`, otherwise
#' a warning is given.
#'
#' @param t2,t1 A pair of vectors giving the time period of the second and
#' first sale, respectively. Usually a vector of dates, but other values are
#' possible if they can be coerced to character vectors and sorted in
#' chronological order (i.e., with [`order()`]).
#' @param p2,p1 A pair of numeric vectors giving the price of the second and
#' first sale, respectively.
#' @param f An optional factor the same length as `t1` and `t2`, or a
#' vector to be turned into a factor, that is used to group sales.
#' @param sparse Should sparse matrices from the \pkg{Matrix} package be used
#' (faster for large datasets), or regular dense matrices (the default)?
#'
#' @returns
#' A function that takes a single argument naming the desired matrix.
#' It returns one of two matrices (\eqn{Z} and \eqn{X}) or two vectors
#' (\eqn{y} and \eqn{Y}), either regular matrices if `sparse = FALSE`, or sparse
#' matrices of class `dgCMatrix` if `sparse = TRUE`.
#'
#' @seealso
#' [rs_pairs()] for turning sales data into sales pairs.
#'
#' @references
#' Bailey, M. J., Muth, R. F., and Nourse, H. O. (1963). A regression method
#' for real estate price index construction.
#' *Journal of the American Statistical Association*, 53(304):933-942.
#'
#' Shiller, R. J. (1991). Arithmetic repeat sales price estimators.
#' *Journal of Housing Economics*, 1(1):110-126.
#'
#' @examples
#' # Make some data
#' x <- data.frame(
#'   date = c(3, 2, 3, 2, 3, 3),
#'   date_prev = c(1, 1, 2, 1, 2, 1),
#'   price = 6:1,
#'   price_prev = 1
#' )
#'
#' # Calculate matrices
#' mat <- with(x, rs_matrix(date, date_prev, price, price_prev))
#' Z <- mat("Z") # Z matrix
#' X <- mat("X") # X matrix
#' y <- mat("y") # y vector
#' Y <- mat("Y") # Y vector
#'
#' # Calculate the GRS index in Bailey, Muth, and Nourse (1963)
#' b <- solve(crossprod(Z), crossprod(Z, y))[, 1]
#' # or b <- qr.coef(qr(Z), y)
#' (grs <- exp(b) * 100)
#'
#' # Standard errors
#' vcov <- rs_var(y - Z %*% b, Z)
#' sqrt(diag(vcov)) * grs # delta method
#'
#' # Calculate the ARS index in Shiller (1991)
#' b <- solve(crossprod(Z, X), crossprod(Z, Y))[, 1]
#' # or b <- qr.coef(qr(crossprod(Z, X)), crossprod(Z, Y))
#' (ars <- 100 / b)
#'
#' # Standard errors
#' vcov <- rs_var(Y - X %*% b, Z, X)
#' sqrt(diag(vcov)) * ars^2 / 100 # delta method
#'
#' # Works with grouped data
#' x <- data.frame(
#'   date = c(3, 2, 3, 2),
#'   date_prev = c(2, 1, 2, 1),
#'   price = 4:1,
#'   price_prev = 1,
#'   group = c("a", "a", "b", "b")
#' )
#'
#' mat <- with(x, rs_matrix(date, date_prev, price, price_prev, group))
#' b <- solve(crossprod(mat("Z"), mat("X")), crossprod(mat("Z"), mat("Y")))[, 1]
#' 100 / b
#'
#' @export
rs_matrix <- function(t2, t1, p2, p1, f = NULL, sparse = FALSE) {
  if (is.null(f)) {
    if (different_lengths(t2, t1, p2, p1)) {
      stop("'t2', 't1', 'p2', and 'p1' must be the same length")
    }
    if (anyNA(t2) || anyNA(t1)) {
      stop("'t2' and 't1' cannot contain NAs")
    }
  } else {
    f <- as.factor(f)
    if (different_lengths(t2, t1, p2, p1, f)) {
      stop("'t2', 't1', 'p2', 'p1', and 'f' must be the same length")
    }
    if (anyNA(t2) || anyNA(t1) || anyNA(f)) {
      stop("'t2', 't1', and 'f' cannot contain NAs")
    }
  }
  z <- rs_z_(t2, t1, f, sparse)
  p2 <- as.numeric(p2)
  p1 <- as.numeric(p1)
  # number of columns that need to be removed for base period
  n <- max(1L, nlevels(f)) * (ncol(z) > 0)
  # return value
  res <- function(matrix = c("Z", "X", "y", "Y")) {
    switch(match.arg(matrix),
      Z = z[, -seq_len(n), drop = FALSE],
      X = rs_x_(z[, -seq_len(n), drop = FALSE], p2, p1),
      y = structure(log(p2 / p1), names = rownames(z)),
      # rowSums() gets the single value in the base period
      # for each group
      Y = -Matrix::rowSums(rs_x_(z[, seq_len(n), drop = FALSE], p2, p1))
    )
  }
  # clean up enclosing environment
  enc <- list(z = z, n = n, p2 = p2, p1 = p1)
  environment(res) <- list2env(enc, parent = getNamespace("rsmatrix"))
  res
}
