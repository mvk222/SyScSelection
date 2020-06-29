#' Finds the d d-dimensional positive vertices for the  given ellipsoid.  A "positive" vertex is one where a  principal axis for the ellipsoid intersects the surface  of the ellipsoid in the direction of the corresponding eigenvector.  (Recall that each of the eigenvectors of  the ellipsoid's shape matrix is collinear with one of the  principal axes.)  This function does not assume that the ellipsoid is centered at the origin.  Because the    direction of each unit eigenvector is arbitrary (i.e., multiplication by -1 still yields a unit eigenvector), a simple algorithm is used to pick a consistent orientation for the vertex points.
#'
#' @param hellip defines the polar vertices
#' @return A d x d array with each column a positive vertex
#' @import pracma
#' @examples 
#' hellip <- hyperellipsoid()
#' vertices(hellip)
#' @export

vertices <- function(hellip){
  # First, we de-mean the ellipsoid, to simplify things
  centerlist <- center_at_origin(hellip)
  hellip1 <- centerlist[[1]]
  mu <- as.matrix(centerlist[[2]])
  sig <- get(hellip1, 'shape')
  size <- get(hellip1, 'size')
  S <- eigen(sig)$vectors
  lambda <- eigen(sig)$values
  d <- max(dim(mu))
  # Algorithmically choose a specific orientation for the eigenvectors,
  # i.e., for the columns of S.  For each column of S (i.e., for each
  # eigenvector), invert the vector if the sum of its components is
  # negative.  This ensures some consistency in the vertices chosen.
  orient <- diag(1,d,d)
  for (j in 1:d){
    if ((t(S[,j])%*%matrix(1,d,1))<0){
      orient[j,j] <- -1
    }
  }
  S <- S%*%orient
  # Calculating vertex lengths
  L <- matrix(0,d,d)
  for (i in 1:d){
    L[i,i] <- sqrt(mrdivide(size,lambda[i]))
  }
  V <- S%*%L
  # Here, we add back in the mean:
  for (i in 1:d){
    V[,i] <- V[,i]+mu[,1]
  }
  return(V)
}
