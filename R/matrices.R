#---- Helper functions (internal) ----
check_many <- function(fun) {
  fun <- match.fun(fun)
  function(...) {
    Reduce("&&", lapply(list(...), fun))
  }
}

no_NAs <- check_many(function(x) !anyNA(x))

all_atomic <- check_many(is.atomic)

all_numeric <- check_many(is.numeric)

all_same_length <- function(...) {
  res <- lengths(list(...))
  all(res == res[1])
}

is_T_or_F <- function(x) {
  length(x) == 1 && is.logical(x) && !is.na(x)
}

distinct <- function(x) {
  length(unique(x))
}

#---- Z matrix (internal) ----
.rs_z <- function(t2, t1, f = NULL, sparse = FALSE) {
  # turn inputs into factors
  lev <- sort(unique(c(as.character(t2), as.character(t1))))
  t2 <- factor(t2, lev)
  t1 <- factor(t1, lev)
  # throw a warning if any t2 < t1
  if (any(as.numeric(t2) <= as.numeric(t1), na.rm = TRUE)) {
    warning("All elements of 't2' should be greater than the corresponding elements in 't1'")
  } 
  # get row names
  nm <- if (length(t2)) {
    if (!is.null(names(t2))) {
      names(t2) 
    } else if (!is.null(names(t1))) {
      names(t1)
    } else if (!is.null(names(f))) {
      names(f)
    } else {
      seq_along(t2)
    }
  }
  # interact with f
  if (!is.null(f)) {
    f <- as.factor(f)
    t2 <- interaction(f, t2)
    t1 <- interaction(f, t1)
  }
  # calculate Z
  if (nlevels(t2) < 2) {
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
  # set useful names
  colnames(z) <- if (nlevels(t2)) levels(t2)
  rownames(z) <- nm
  # remove model.matrix attributes
  attributes(z)[c('assign', 'contrasts')] <- NULL
  z
}

#---- X matrix (internal) ----
.rs_x <- function(z, p2, p1) (z > 0) * p2 - (z < 0) * p1

#---- All matrices ----
rs_matrix <- function(t2, t1, p2, p1, f = NULL, sparse = FALSE) {
  # check input
  stopifnot(
    "'t2' and 't1' must be atomic vectors" = all_atomic(t2, t1),
    "'p2' and 'p1' must be numeric vectors" = all_numeric(p2, p1), 
    "'t2', 't1', p2', and 'p1' must be the same length" = all_same_length(p2, p1, t2, t1),
    "'f' must be an atomic vector" = is.atomic(f), 
    "'f' must be either NULL or the same length as 't2' and 't1'" = is.null(f) || all_same_length(f, t2, t1),
    "'t2', 't1', and 'f' cannot contain NAs" = no_NAs(t2, t1, f),
    "'sparse' must be TRUE or FALSE" = is_T_or_F(sparse)
  )
  # make the z matrix (including first column)
  z <- .rs_z(t2, t1, f, sparse)
  # number of columns that need to be removed for base period
  n <- max(distinct(f), min(1, ncol(z)))
  # return value
  function(matrix = c("Z", "X", "y", "Y")) {
    switch(match.arg(matrix),
           Z = z[, -seq_len(n), drop = FALSE],
           X = .rs_x(z[, -seq_len(n), drop = FALSE], p2, p1),
           y = setNames(log(p2 / p1), rownames(z)),
           Y = -rowSums(.rs_x(z[, seq_len(n), drop = FALSE], p2, p1)))
  }
}