rs_var <- function(u, X, Z, ids = seq(nrow(X)), df) {
  # check input
  stopifnot("'u' must be a column vector" = 
              length(dim(u)) == 2L && ncol(u) == 1L,
            "'X' must be a matrix" = 
              length(dim(X)) == 2L,
            "'X' and 'u' must have the same number of rows" = 
              nrow(u) == nrow(X),
            "'X' and 'Z' must have the same dimension" =
              missing(Z) || identical(dim(X), dim(Z)),
            "'ids' must be an atomic vector" = 
              is.atomic(ids),
            "'ids' must be the same length as 'u'" = 
              length(ids) == nrow(u),
            "'df' must be a length 1 numeric vector" = 
              length(df) == 1L && is.vector(df, "numeric"))
  # the meat
  ug <- split.data.frame(u, ids)
  A <- if (missing(Z)) X else Z
  Ag <- split.data.frame(A, ids)
  V <- lapply(seq_along(ug), function(i) tcrossprod(crossprod(Ag[[i]], ug[[i]])))
  V <- Reduce(`+`, V)
  # the bread
  B <- solve(if (missing(Z)) crossprod(X) else crossprod(Z, X))
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