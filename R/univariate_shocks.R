#' Calculates 2d d-dimensional univariate shocks (up and down in each of the d dimensions) based on the given  ellipsoid. Univariate shocks are points on the surface of the ellipsoid that differ from the center of the  ellipsoid in only one dimension. Thus, for an ellipsoid centered at the origin, only one element of a d-dimensional shock will be non-zero.This function does not assume that the ellipsoid is centered at the origin.
#'
#' @param hellip the basis for the shocks; it must have measurable width in every dimension
#' @return A d x 2d array, [dx2d], with each column a shock; the first d columns are positive univariate shocks, and final d columns are matching negative univariate shocks
#' @import pracma
#' @examples 
#' hellip <- hyperellipsoid()
#' univariate_shocks(hellip)
#' @export

univariate_shocks <- function(hellip) {
  centerlist <- center_at_origin(hellip)
  hellip1 <- centerlist[[1]]
  mu <- centerlist[[2]]
  sig <- hellip$shape
  c <- hellip$size
  d <- length(mu)
  dx2d <- matrix(0,d,2*d)
  for (i in 1:d){
    dx2d[i,i] <- sqrt(mrdivide(c,sig[i,i]))
    dx2d[i,2*i] <- -sqrt(mrdivide(c,sig[i,i]))
  }
  for (i in 1:2*d){
    dx2d[,i] <- dx2d[,i] + mu
  }
  return(dx2d)
}
