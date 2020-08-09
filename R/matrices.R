#---- Z matrix ----
rs_z <- function(t2, t1, f, sparse = FALSE) {
  # check input
  stopifnot(
    "'t2' and 't1' must be atomic vectors" =
      is.atomic(t2) && is.atomic(t1), 
    "'t2' and 't1' must be the same length" = 
      length(t2) == length(t1),
    "'f' must be an atomic vector" = 
      missing(f) || is.atomic(f),
    "'f' must be the same length as 't2' and 't1'" = 
      missing(f) || length(f) == length(t2),
    "'t2' and 't1' cannot contain NAs" = 
      !anyNA(t2) && !anyNA(t1),
    "'f' cannot contain NAs" = 
      missing(f) || !anyNA(f),
    "'sparse' must be TRUE or FALSE" = 
      length(sparse) == 1L && is.logical(sparse) && !is.na(sparse)
  )
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
  if (!missing(f)) {
    f <- as.factor(f)
    t2 <- f:t2
    t1 <- f:t1
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

#---- X matrix ----
rs_x <- function(p2, p1, z) {
  # check input
  stopifnot(
    "'p2' and 'p1' must be numeric vectors" = 
      is.vector(p2, "numeric") && is.vector(p1, "numeric"), 
    "'p2' and 'p1' must be the same length" =
      length(p2) == length(p1),
    "'z' must be a matrix" = 
      length(dim(z)) == 2L, 
    "Each column in 'z' must be the same length as 'p2' and 'p1'" = 
      length(p2) == nrow(z)
  )
  # return X matrix
  (z > 0) * p2 - (z < 0) * p1
}