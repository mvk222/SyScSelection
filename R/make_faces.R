#' Fills a mesh with d-dimensional points representing all non-edge face points of a d-dimensional cube encompassing a  d-dimensional unit spheroid.
#'
#' @param d The number of dimensions for the unit spheroid
#' @param phi Fineness of the mesh along each dimension of the 2D face
#' @param normalize Whether to scale the corner points onto the sphere or not
#' @return A d x d*(d-1)*2^(d-3)*(phi-2)^2 array of face points
#' @import pracma
#' @export

make_faces <- function(d,phi,normalize){
  face_template <-  fill_adj_2Dface(d, phi)
  face_pts <-  d*(d-1)*2^(d-3)*(phi-2)*(phi-2)
  face_mesh <-  matrix(1,d, face_pts)%*%333
  corner_mesh <- make_corners(d-2, FALSE)
  corner_rows <- length(corner_mesh[1,])
  corner_cols <-  length(corner_mesh[,1])
  patch <- matrix(0,d, corner_rows)
  cursor <- 1
  for (active_edge1 in 1:d){
    for (active_edge2 in (active_edge1+1):d){
      for (p1 in 2:(phi-1)){
        face_vals <- face_template(p1,p1,d)%*%matrix(1,2,corner_rows)
        for (p2 in 2:(phi-1)){
          face_vals[2,1:corner_rows] <- face_template(p2, p2, d)%*% matrix(1,1, corner_rows)
          cmax <-  cursor+corner_rows-1
          patch[1:(active_edge1-1), 1:corner_rows] <- corner_mesh[1:(active_edge1-1), 1:corner_rows]
          patch[active_edge1, 1:corner_rows] <- face_vals[1,]
          patch[(active_edge1+1):(active_edge2-1), 1:corner_rows] <- corner_mesh[active_edge1:(active_edge2-2), 1:corner_rows]
          patch[active_edge2, 1:corner_rows] <- face_vals[2,]
          patch[(active_edge2+1):d, 1:corner_rows] <- corner_mesh[(active_edge2-1):corner_cols, 1:corner_rows]
          if (normalize) {
            patch <- mrdivide(patch,norm(patch[, 1],type="2"))
          }
          face_mesh[1:d, cursor:cmax] <- patch
          cursor <- cursor + corner_rows
        }
      }
    }
  }
  return(face_mesh)
}