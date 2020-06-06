#' Generates a Cartesian mesh of d-dimensional scenarios based on the given ellipsoid.  This function does not assume that the ellipsoid is centered at the origin.
#'
#' @param d The number of dimensions for the unit spheroid
#' @param phi The scalar fineness of the mesh
#' @param normalize Whether to normalize points from the cube onto the sphere or not (TRUE/FALSE)
#' @return A d x N array with each column a scenario

spheroid_mesh <- function(d,phi,normalize){
  # Memorize the raw 2D grid of points
  face_template <-  fill_adj_2Dface(d, phi)
  # How many shocks will we get
  listmeshsize <- calc_mesh_size(phi, d)
  mesh_size <- listmeshsize[[1]]
  corn_pts <- listmeshsize[[2]]
  edge_pts <- listmeshsize[[3]]
  face_pts <- listmeshsize[[4]]
  #print(paste("d =",d,",phi =",phi,",mesh_size =",mesh_size, ",corn_pts =",corn_pts,",face_pts =", face_pts,sep = " "))
  # The basic Cartesian mesh:
  # Start with a mesh filled with 999s.  Why 999s?  Because 999s will stand out from true mesh points, which are within the unit hypercube.
  mesh <- matrix(1,d,mesh_size)*999
  # Insert the corner points for the first corn_pts columns:
  corn_mesh <- make_corners(d, normalize)
  mesh[,1:corn_pts] <- corn_mesh
  # Insert the non-corner edge points for the next edge_pts columns:
  edge_mesh <- make_edges(d,phi,normalize)
  edge_pts <- length(edge_mesh[1,])
  cursor_min <- corn_pts+1
  cursor_max <- corn_pts+edge_pts
  mesh[,cursor_min:cursor_max] <- edge_mesh
  # Insert the non-edge face points for the next face_pts columns:
  face_mesh <- make_faces(d,phi,normalize)
  face_pts <- length(face_mesh[1,])
  cursor_min <- cursor_max + 1
  cursor_max <- cursor_max+face_pts
  mesh[,cursor_min:cursor_max] <- face_mesh
  return(mesh)
}
