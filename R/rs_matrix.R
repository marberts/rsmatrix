#---- Helper functions (internal) ----
different_lengths <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
}

#---- Z matrix (internal) ----
rs_z_ <- function(t2, t1, f = NULL, sparse = FALSE) {
  # coerce t2 and t1 into characters prior to taking the union
  # so that both dates and factors are treated the same
  lev2 <- as.character(unique(t2))
  lev1 <- as.character(unique(t1))
  lev <- sort.int(unique(c(lev2, lev1))) # usually faster than base::union()
  t2 <- factor(t2, lev)
  t1 <- factor(t1, lev)
  if (any(as.numeric(t2) <= as.numeric(t1))) {
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
  }

  # calculate Z
  if (nlevels(t2) < 2L) {
    # return a nx1 matrix of 0's if there's only one level
    # return a 0x0 matrix if there are no levels
    z <- matrix(rep.int(0, length(t2)), ncol = nlevels(t2))
    if (sparse) {
      z <- as(as(z, "generalMatrix"), "CsparseMatrix")
    }
  } else {
    # model matrix otherwise
    mm <- if (sparse) sparse.model.matrix else model.matrix
    z <- mm(~ t2 - 1, contrasts.arg = list(t2 = "contr.treatment")) -
      mm(~ t1 - 1, contrasts.arg = list(t1 = "contr.treatment"))
  }
  if (nlevels(t2) > 0L) {
    colnames(z) <- levels(t2)
    rownames(z) <- nm
  }
  # remove model.matrix attributes
  attributes(z)[c("assign", "contrasts")] <- NULL
  z
}

#---- X matrix (internal) ----
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
