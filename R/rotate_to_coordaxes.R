#' Rotates the ellipsoid (hellip) so its principal axes align with the coordinate axes.  Both ellipsoids are centered at the origin.  Note that there are (2^d)*d! valid ways to rotate the ellipsoid to the axes. This algorithm does not prescribe which solution will be provided.
#' @param hellip The shape to be rotated, must be centered at the origin
#' @return A list of: hellip2 - A new hyperellipsoid, rotated to the coordinate axes and tfm - the transformation matrix that creates the rotation
#' @import pracma

rotate_to_coordaxes <- function(hellip){
  V <- vertices(hellip)
  d <- max(dim(V))
  V2 <- matrix(0,d,d)
  for (i in 1:d){
    V2[i,i] <- norm(V[,i],type="2")
  }
  tfm <- mrdivide(V2,V)
  hellip2 <- transform_ellipsoid(hellip, tfm)
  return(list(hellip2,tfm))
}
