#' Calculates the size paramater for a d-dimensional hyperellipsoid conforming to a Student's t distribution.
#' @param prob The target probability threshold
#' @param d Number of dimensions in the multivariate distribution
#' @param nu Degrees of freedom parameter for the t distribution
#' @return The appropriate (scalar) size parameter
#' @import stats
#' @examples sizeparam_t_distn(0.95, 6, 5)
#' @export

sizeparam_t_distn <- function(prob,d,nu){
  return(d*qf(prob,d,nu))
}