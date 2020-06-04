#' Stretches the ellipsoid (hellip) to the unit spheroid of the same dimension.  Both the input ellipsoid and unit spheroid are centered at the origin.
#'
#' @param hellip The original shape to be stretched
#' @return A list of: hellip1 - a new unit spheroid, mapped from the ellipsoid and tfm - transformation matrix that creates the stretching

stretch_to_unitspheroid <- function(hellip){
  V <- vertices(hellip)
  tfm <- solve(V)
  hellip1 <- transform_ellipsoid(hellip, tfm)
  return(list(hellip1,tfm))
}
