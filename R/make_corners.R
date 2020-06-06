#' Fills a mesh (corn_mesh) with d-dimensional points representing all corners of a d-dimensional cube encompassing a d-dimensional unit spheroid.
#'
#' @param d The number of dimensions for the unit spheroid
#' @param normalize Whether to scale the corner points onto the sphere or not
#' @return A d x 2^d array of corner points
#' @import pracma

make_corners <- function(d,normalize){
  corn_mesh <-  fill_corners(matrix(1,d,2^d)*111, matrix(0,d,1), 0, 1)[[1]]
  if (normalize) {
  # Scale every point to unit length:
  scal <-  norm(corn_mesh[,1],type="2")
  corn_mesh <-  corn_mesh/scal
  }
  return(corn_mesh)
}
