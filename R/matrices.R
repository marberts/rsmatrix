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
  if (sparse) {
    # if sparse is TRUE then use the sparse.model.matrix function
    # Matrix needs to be installed to make a spare matrix
    if (requireNamespace('Matrix', quietly = TRUE)) {
      mm <- Matrix::sparse.model.matrix
    } else {
      stop('The Matrix library is not installed.')
    }
    # otherwise use the regular dense model matrix
  } else {
    mm <- stats::model.matrix
  }
  # turn inputs into factors
  lev <- sort(unique(c(as.character(t2), as.character(t1))))
  # throw an error if there are fewer than 2 levels
  if (length(lev) < 2L) stop("There must be at least two levels in 't2' and 't1'")
  t2 <- factor(t2, lev)
  t1 <- factor(t1, lev)
  # throw a warning if any t2 < t1
  if (any(as.numeric(t2) <= as.numeric(t1))) {
    warning("All elements of 't2' should be greater than the corresponding elements in 't1'")
  } 
  # interact with f
  if (!missing(f)) {
    f <- as.factor(f)
    t2 <- f:t2
    t1 <- f:t1
  }
  # calculate Z
  z <- mm(~ t2 - 1) - mm(~ t1 - 1)
  # set useful column names
  colnames(z) <- levels(t2)
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