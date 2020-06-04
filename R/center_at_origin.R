#' Creates a new ellipsoid object equivalent to the given hyperellipsoid (hellipse), but centered at the origin.
#'
#' @param hellip The original object, to be shifted
#' @return list of two: hellip2 - the re-centered hyperellipsoid and mu - the amount of the translation

center_at_origin <- function(hellip){
  mu <- get(hellip,"center")
  sig <- get(hellip,"shape")
  c <- get(hellip,"size")
  hellip2 <- hyperellipsoid(matrix(0, length(mu), 1), sig, c)
  return(list(hellip2,mu))
}
