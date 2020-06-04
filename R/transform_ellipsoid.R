#' Applies the given linear transformation, tfm, to the given ellipsoid.  The ellipsoid (hellip) must be centered at the origin.
#'
#' @param hellip The original shape to be transformed
#' @param tfm A d x d linear transformation matrix
#' @return A transformed ellipsoid, centered at the origin

transform_ellipsoid <- function(hellip,tfm){
  V <- vertices(hellip)
  V2 <- tfm%*%V
  hellip1 <- make_ellipsoid_from_vertices(V2, get(hellip, 'size'))
  return(hellip1)
}
