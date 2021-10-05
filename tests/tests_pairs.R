#---- Tests for finding previous values ----
prev2 <- function(x) {
  dr <- match(x, sort(unique(x)))
  match(pmax(dr - 1L, 1L), dr, incomparables = NA)
}

set.seed(1432)
x <- sample(1e3)
y <- replace(x, sample(1e3, 10), NA)

stopifnot(
  exprs = {
    all.equal(prev(x), prev2(x))
    all.equal(prev(y), prev2(y))
  },
  local = getNamespace("rsmatrix")
)
