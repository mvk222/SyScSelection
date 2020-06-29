#' Creates a new base-b sequence of a designated length
#'
#' @param k The integer to expand
#' @param b Base for integer expansions used in the sequence
#' @return The expansion of the integer k
#' @import pracma

new_baseb_expansion <- function(k, b) {
  if (k > 0) {
    jmax <- trunc(mrdivide(log(k) , log(b)))
    a <- matrix(0, 1, jmax + 1)
    q <- b ^ jmax
    for (j in 1:(jmax + 1)) {
      a[j] <- floor(mrdivide(k , q))
      k <- k - q %*% a[j]
      q <- mrdivide(q,b)
    }
  } else {
    a <- matrix(0, 1, 1)
  }
  return(a)
}
