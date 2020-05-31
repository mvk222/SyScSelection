#' Generates a base-b Van der Corput sequence with k elements
#'
#' @param k Number of elements in the sequence
#' @param b Base for integer expansions used in the sequence
#' @return k-dimensional array containing the sequence
#' @import pracma
#' @export

vandercorput <- function(k,b){
  if (k != floor(k) || (k<0)){
    stop("k is NOT a non-negative integer.")
  }
  if (b != floor(b) || (b<2)){
    stop("b is NOT a positive integer greater than 1.")
  }
  s <- matrix(0,k+1,1)
  for (i in 1:k){
    xi <- 0
    q <- 1/b
    a <- baseb_expansion(i,b)
    m <- length(a)
    for (j in 1:m){
      xi <- xi + q%*%a[m-j+1]
      q <- q/b
    }
    s[i+1] <- xi
  }
  return(s)
}
