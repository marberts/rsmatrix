#---- Helper functions (internal) ----
different_lengths <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
}

rs_z_ <- function(t2, t1, f = NULL, sparse = FALSE) {
  # coerce t2 and t1 into characters prior to taking the union
  # so that both dates and factors are treated the same
  lev2 <- as.character(unique(t2))
  lev1 <- as.character(unique(t1))
  lev <- sort.int(unique(c(lev2, lev1))) # usually faster than base::union()
  t2 <- factor(t2, lev)
  t1 <- factor(t1, lev)
  if (any(unclass(t2) <= unclass(t1))) {
    warning("all elements of 't2' should be greater than the corresponding ",
            "elements in 't1'")
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
    res <- sparseMatrix(rep.int(i, 2), c(t2, t1),
                        x = rep(c(1, -1), each = length(i)),
                        dims = dims)
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

rs_x_ <- function(z, p2, p1) (z > 0) * p2 - (z < 0) * p1

#---- All matrices ----
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
    switch(
      match.arg(matrix),
      Z = z[, -seq_len(n), drop = FALSE],
      X = rs_x_(z[, -seq_len(n), drop = FALSE], p2, p1),
      y = structure(log(p2 / p1), names = rownames(z)),
      # rowSums() gets the single value in the base period
      # for each group
      Y = -rowSums(rs_x_(z[, seq_len(n), drop = FALSE], p2, p1))
    )
  }
  # clean up enclosing environment
  enc <- list(z = z, n = n, p2 = p2, p1 = p1)
  environment(res) <- list2env(enc, parent = getNamespace("rsmatrix"))
  res
}
