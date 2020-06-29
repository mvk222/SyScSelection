#' Calculates the distance between the given point and the given hyperellipsoid (hellipse), measured along a line connecting the point with the center of the ellipsoid.  If the given point is interior to the ellipsoid, then the reported distance is negative.   If the point is on the ellipsoid, the distance is zero. If the point lies outside the ellipsoid, the distance is postive.
#' @param hellip The shape to which distance is measured
#' @param point Where distance is measured from
#' @return The Euclidean distance between the point and the corresponding spot on the surface of hellip
#' @expamples distance_to_ellipsoid(hellip,point)
#' @export

distance_to_ellipsoid <- function(hellip,point){
  # First, move the ellipsoid to the origin:
  centerlist <- center_at_origin(hellip)
  hellip2 <- centerlist[[1]]
  mu <- centerlist[[2]]
  # Then, move the point to the origin:
  d <- length(point)
  point2 <- matrix(0,d,1)
  for (i in 1:d){
    point2[i,1] <- point[i,1]-mu[i,1]
  }
  # Find the multiplicative scalar that will scale our point2 so that it falls on the surface of the ellipsoid
  sig2 <- get(hellip2,"shape")
  c2 <- get(hellip2,"size")
  scalar <- sqrt(c2/(t(point2)%*%sig2%*%point2))
  # How far is the original point from its scaled equivalent on the surface?
  distance <- norm(point2, type="2") - norm((scalar%*%point2), type="2")
  return(distance)
}
