#---- Helper functions ----
sss <- function(n, k, g) {
  g / (g - 1) * (n - 1) / (n - k)
}

#---- Variance matrix ----
rs_var <- function(u, Z, X = Z, ids = seq_len(nrow(X)), df = sss(nrow(X), ncol(X), nlevels(ids))) {
  ids <- as.factor(ids)
  # the meat
  ug <- split.data.frame(u, ids)
  Zg <- split.data.frame(Z, ids)
  V <- lapply(seq_along(ug), 
              function(i) {
                tcrossprod(crossprod(Zg[[i]], ug[[i]]))
              })
  V <- Reduce(`+`, V)
  # the bread
  B <- solve(crossprod(Z, X))
  # put the sandwich together
  vcov <- tcrossprod(B %*% V, B)
  df * vcov
}