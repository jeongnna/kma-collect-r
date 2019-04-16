na_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}


na_interpolate <- function(x) {
  is_not_na <- function(x) {
    !is.na(x)
  }
  n <- length(x)
  if (is.na(x[1])) {
    first_idx <- detect_index(x, is_not_na)
    x[1:first_idx] <- x[first_idx]
  }
  if (is.na(x[n])) {
    last_idx <- detect_index(x, is_not_na, .dir = "backward")
    x[last_idx:n] <- x[last_idx]
  }
  i <- 2
  while (i < n) {
    if (is.na(x[i])) {
      j <- detect_index(x[(i + 1):n], is_not_na)
      x[(i - 1):(i + j)] <- seq(x[i - 1], x[i + j], length.out = j + 2)
      i <- i + j
    }
    i <- i + 1
  }
  x
}
