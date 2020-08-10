#---- Z matrix ----
rs_z_ <- function(t2, t1, f = NULL, sparse = FALSE) {
  # make sure Matrix is installed if spare == TRUE
  if (sparse && !requireNamespace('Matrix', quietly = TRUE)) {
    stop("The 'Matrix' library is not installed.")
  }
  # turn inputs into factors
  lev <- sort(unique(c(as.character(t2), as.character(t1))))
  t2 <- factor(t2, lev)
  t1 <- factor(t1, lev)
  # throw a warning if any t2 < t1
  if (any(as.numeric(t2) <= as.numeric(t1))) {
    warning("All elements of 't2' should be greater than the corresponding elements in 't1'")
  } 
  # get row names
  nm <- if (length(t2)) {
    if (!is.null(names(t2))) {
      names(t2) 
    } else if (!is.null(names(t1))) {
      names(t1)
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
  if (nlevels(t2) < 2L) {
    # return a nx1 matrix of 0's if there's only one level
    # return a 0x0 matrix if there are no levels
    z <- matrix(rep(0, length(t2)), ncol = nlevels(t2))
    if (sparse) z <- methods::as(z, "dgCMatrix")
  } else {
    # model matrix otherwise
    mm <- if (sparse) Matrix::sparse.model.matrix else stats::model.matrix
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

#---- All matrices ----
rs_matrix <- function(t2, t1, p2, p1, f = NULL, sparse = FALSE) {
  # check input
  stopifnot(
    "'t2' and 't1' must be atomic vectors of the same length" =
      is.atomic(t2) && is.atomic(t1) && length(t2) == length(t1), 
    "'p2' and 'p1' must be numeric vectors the same length as 't2' and 't1'" =
      is.vector(p2, "numeric") && is.vector(p1, "numeric") && 
      length(p2) == length(p1) && length(p2) == length(t2),
    "'f' must be an atomic vector, either NULL or the same length as 't2' and 't1'" = 
      is.atomic(f) && (is.null(f) || length(f) == length(t2)),
    "'t2', 't1', and 'f' cannot contain NAs" = 
      !anyNA(t2) && !anyNA(t1) && !anyNA(f),
    "'sparse' must be TRUE or FALSE" = 
      length(sparse) == 1L && is.logical(sparse) && !is.na(sparse)
  )
  z <- rs_z_(t2, t1, f, sparse)
  x <- (z > 0) * p2 - (z < 0) * p1
  n <- max(length(unique(f)), min(1L, length(z)))
  y <- -(if (sparse) Matrix::rowSums else rowSums)(x[, seq_len(n), drop = FALSE])
  list(Z = z[, -seq_len(n), drop = FALSE], 
       X = x[, -seq_len(n), drop = FALSE], 
       Y = y,
       y = log(p2) - log(p1))
}