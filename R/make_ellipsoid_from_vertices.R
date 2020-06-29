#' Constructs a new d-dimensional ellipsoid with the given "positive vertices", and size parameter, c.  The constructed ellipsoid is centered at the origin.Note that the input vertices (i.e., the columns of V) must therefore be orthogonal vectors, themselves centered at the origin.The size parameter, c, may be needed because the points alone only determine the eigenvalues up to a positive constant. For vertices which fall on the constructed ellipsoid, choose as the size parameterc = 1.The new ellipsoid is centered at the origin.
#'
#' @param V A d x d array of positive vertices (in columns)
#' @param c The size parameter of the new ellipsoid
#' @return A new ellipsoid, centered at the origin, with the given vertices
#' @import pracma
#' @examples 
#' hellip <- hyperellipsoid()
#' V <- vertices(hellip)
#' c <- 4
#' make_ellipsoid_from_vertices(V,c)
#' @export

make_ellipsoid_from_vertices <- function(V,c){
  d <- max(dim((V)))
  L <- matrix(0,d,d)
  S <- matrix(0,d,d)
  lambda <- matrix(0,d,d)
  for (i in 1:d){
    # The length of each vertex vector:
    L[i,i] <-  norm(V[,i],type="2")
    # Set of orthonormal eigenvectors:
    S[,i] = V[,i]/L[i,i]
    # Lambda is the matrix of eigenvalues (note the scaling by c):
    lambda[i,i] = mrdivide(c,(L[i,i]%*%L[i,i]))
    # Test that the vectors are all orthogonal:
    j = i+1
    while (j <= d){
      Vi <- V[,i]
      Vj <- V[,j]
      cosine <- mrdivide((t(Vi)%*%Vj),(norm(Vi,type="2")%*%norm(Vj,type="2")))
      if (abs(cosine)> 0.000001){
        stop(paste("Vertices not orthogonal", i, j, sep = " "))
      }
      j <- j + 1
    }
  }
  hellip <- hyperellipsoid(matrix(0,d, 1), S%*%lambda%*%t(S), c)
  return(hellip)
}
