#' Calculates the size paramater for a d-dimensional hyperellipsoid conforming to a normal (i.e., Gaussian) distribution.
#'
#' @param prob The target probability threshold
#' @param d Number of dimensions in the multivariate distribution
#' @return The appropriate (scalar) size parameter
#' @import stats
#' @examples sizeparam_normal_distn(0.95, 6)
#' @export

sizeparam_normal_distn <- function(prob,d){
  return(qchisq(prob,d))
}
