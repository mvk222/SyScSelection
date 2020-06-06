#' Fills a mesh with d-dimensional points representing all non-corner edge points of a d-dimensional cube encompassing a  d-dimensional unit spheroid.
#'
#' @param d The number of dimensions for the unit spheroid
#' @param phi Fineness of the mesh along the edge (i.e., the total number of points, "including" the corners)
#' @param normalize Whether to scale the corner points onto the sphere or not
#' @return A d x d*2^(d-1)*(phi-2) array of edge points
#' @import pracma

make_edges <- function(d,phi,normalize){
  face_template <-  fill_adj_2Dface(d, phi)
  edge_pts <-  d*2^(d-1)*(phi-2)
  edge_mesh = matrix(1, d, edge_pts)*222
  corner_mesh <-  make_corners(d-1, FALSE)
  corner_rows <-  length(corner_mesh[1,])
  corner_cols <-  length(corner_mesh[,1])
  patch <- matrix(0,d,corner_rows)
  cursor <- 1
  for (active_edge in 1:d){
    for (p in 2:(phi-1)){
      edge_vals <-  face_template[1, p, d]%*%matrix(1, 1, corner_rows)
      cmax <-  cursor+corner_rows-1
      patch[1:(active_edge-1), 1:corner_rows] <-  corner_mesh[1:(active_edge-1), 1:corner_rows]
      patch[active_edge, 1:corner_rows] <- edge_vals[1,]
      if (active_edge < d){
        patch[(active_edge+1):d, 1:corner_rows] <- corner_mesh[active_edge:corner_cols, 1:corner_rows]
      }
      if (normalize){
        patch <- patch/norm(patch[,1],type="2")
      }
      edge_mesh[1:d, cursor:cmax] <-  patch
      cursor <- cursor + corner_rows
    }
  }
  return(edge_mesh)
}
