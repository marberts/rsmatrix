#---- Helper functions (internal) ----
different_lengths <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
}

#---- Z matrix (internal) ----
.rs_z <- function(t2, t1, f = NULL, sparse = FALSE) {
  lev <- sort(unique(c(as.character(t2), as.character(t1))))
  t2 <- factor(t2, lev)
  t1 <- factor(t1, lev)
  # something is probably wrong if t2 <= t1
  if (any(as.numeric(t2) <= as.numeric(t1), na.rm = TRUE)) {
    warning(gettext("all elements of 't2' should be greater than the corresponding elements in 't1'"))
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
  # interact with f
  if (!is.null(f)) {
    f <- as.factor(f)
    t2 <- interaction(f, t2)
    t1 <- interaction(f, t1)
  }
  # calculate Z
  if (nlevels(t2) < 2L) {
    # return a nx1 matrix of 0's if there's only one level
    # return a 0x0 matrix if there are no levels
    z <- matrix(rep(0, length(t2)), ncol = nlevels(t2))
    if (sparse) z <- as(z, "dgCMatrix")
  } else {
    # model matrix otherwise
    mm <- if (sparse) sparse.model.matrix else model.matrix
    z <- mm(~ t2 - 1, contrasts.arg = list(t2 = "contr.treatment")) - 
      mm(~ t1 - 1, contrasts.arg = list(t1 = "contr.treatment"))
  }
  if (nlevels(t2)) {
    colnames(z) <- levels(t2)
    rownames(z) <- nm
  }
  # remove model.matrix attributes
  attributes(z)[c('assign', 'contrasts')] <- NULL
  z
}

#---- X matrix (internal) ----
.rs_x <- function(z, p2, p1) (z > 0) * p2 - (z < 0) * p1

#---- All matrices ----
rs_matrix <- function(t2, t1, p2, p1, f = NULL, sparse = FALSE) {
  if (is.null(f)) {
    if (different_lengths(t2, t1, p2, p1)) {
      stop(gettext("'t2', 't1', 'p2', and 'p1' must be the same length"))
    }
    if (anyNA(t2) || anyNA(t1)) {
      stop(gettext("'t2' and 't1' cannot contain NAs"))
    }
  } else {
    if (different_lengths(t2, t1, p2, p1, f)) {
      stop(gettext("'t2', 't1', 'p2', 'p1', and 'f' must be the same length"))
    }
    if (anyNA(t2) || anyNA(t1) || anyNA(f)) {
      stop(gettext("'t2', 't1', and 'f' cannot contain NAs"))
    }
    f <- as.factor(f)
  }
  z <- .rs_z(t2, t1, f, sparse)
  # number of columns that need to be removed for base period
  n <- max(nlevels(f), min(1L, ncol(z)))
  # return value
  function(matrix = c("Z", "X", "y", "Y")) {
    switch(match.arg(matrix),
           Z = z[, -seq_len(n), drop = FALSE],
           X = .rs_x(z[, -seq_len(n), drop = FALSE], p2, p1),
           y = structure(log(p2 / p1), names = rownames(z)),
           Y = -rowSums(.rs_x(z[, seq_len(n), drop = FALSE], p2, p1)))
  }
}