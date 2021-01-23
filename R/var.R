#---- Helper functions ----
is_matrix <- function(x) {
  is.matrix(x) || is(x, "Matrix")
}

is_column_vector <- function(x) {
  is_matrix(x) && ncol(x) == 1
}

sss <- function(n, k, g) {
  g / (g - 1) * (n - 1) / (n - k)
}

#---- Variance matrix ----
rs_var <- function(u, Z, X = Z, ids = seq_len(nrow(X)), df = sss(nrow(X), ncol(X), distinct(ids))) {
  # check input
  stopifnot("'u' must be a column vector" = is_column_vector(u),
            "'Z' must be a matrix" = is_matrix(Z),
            "'Z' and 'u' must have the same number of rows" = identical(nrow(u), nrow(Z)),
            "'Z' and 'X' must have the same dimensions" = identical(dim(X), dim(Z)),
            "'ids' must be an atomic vector" = is.atomic(ids),
            "'ids' must be the same length as 'u'" = all_same_length(ids, u))
  # the meat
  ug <- split.data.frame(u, ids)
  Zg <- split.data.frame(Z, ids)
  V <- lapply(seq_along(ug), function(i) tcrossprod(crossprod(Zg[[i]], ug[[i]])))
  V <- Reduce(`+`, V)
  # the bread
  B <- solve(crossprod(Z, X))
  # put the sandwich together
  vcov <- tcrossprod(B %*% V, B)
  df * vcov
}