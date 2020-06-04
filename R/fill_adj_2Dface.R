#' Creates a phi x phi grid (i.e., the mesh on a single two-dimensional face of a larger hypercube) of d-dimensional points, where the regularity of the grid has been adjusted to avoid  clustering in the corners.
#'
#' @param d The number of dimensions for the unit spheroid
#' @param phi Fineness of the mesh along each dimension of the 2D face
#' @return A phi x phi x d array of points.  The points (each facemesh2D(i,j,:)) are identically equal to one in the first d-2 dimensions, so that the mesh varies only in the final two dimensions.
#' @import pracma

fill_adj_2Dface <- function(d,phi){
  facemesh2D <- array(0, dim=c(phi, phi, d))
  # perim holds an example of the points along one of the four edges
  # of the square face. There are phi points along each edge.
  # Since the four edges are symmetrical, we only need to calculate
  # one edge example.
  perim = array(1,dim=c(phi, d))
  z_one <- perim[1,]
  z_phi <- perim[phi,]
  for (p in 2:(phi-1)){
    beta <- fill_adj_2Dface_beta(p, phi, z_one, z_phi)
    perim[p,] <- beta%*%z_one + (1-beta)%*%z_phi
  }
  # Interpolating from the edge points in perim to get the interior points of each row/column:
  for (i in 1:phi){
    z_one <- array(1,dim=c(1,d))
    z_phi <- array(1,dim=c(1,d))
    z_one[1,d] <- -1
    z_one[1,d-1] <- perim[i,d]
    z_phi[1,d-1] <- perim[i,d]
    for (j in 1:phi){
      beta <- fill_adj_2Dface_beta(j, phi, z_one, z_phi)
      facemesh2D[i,j,] = beta%*%z_one + (1-beta)%*%z_phi
    }
  }
  # Now set the rows to achieve symmetry with the columns:
  for (i in 1:phi){
    for (j in 1:phi){
      facemesh2D[j,i,d-1] <- facemesh2D[i,j,d]
    }
  }
  return(facemesh2D)
}
