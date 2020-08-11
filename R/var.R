rs_var <- function(u, Z, X = Z, ids = seq_len(nrow(X)), df) {
  # check input
  stopifnot("'u' must be a column vector" = 
              length(dim(u)) == 2L && ncol(u) == 1L,
            "'Z' must be a matrix" = 
              length(dim(Z)) == 2L,
            "'Z' and 'u' must have the same number of rows" = 
              nrow(u) == nrow(Z),
            "'Z' and 'X' must have the same dimensions" =
              identical(dim(X), dim(Z)),
            "'ids' must be an atomic vector" = 
              is.atomic(ids),
            "'ids' must be the same length as 'u'" = 
              length(ids) == nrow(u),
            "'df' must be a length 1 numeric vector" = 
              missing(df) || length(df) == 1L && is.vector(df, "numeric")
            )
  # the meat
  ug <- split.data.frame(u, ids)
  Zg <- split.data.frame(Z, ids)
  V <- lapply(seq_along(ug), function(i) tcrossprod(crossprod(Zg[[i]], ug[[i]])))
  V <- Reduce(`+`, V)
  # the bread
  B <- solve(crossprod(Z, X))
  # put the sandwich together
  vcov <- tcrossprod(B %*% V, B)
  # df correction
  if (missing(df)) {
    n <- nrow(X)
    k <- ncol(X)
    g <- length(unique(ids))
    return(g / (g - 1) * (n - 1) / (n - k) * vcov) # stata's df correction
  } else {
    return(df * vcov)
  }
}