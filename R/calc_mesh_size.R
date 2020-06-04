#' Calculates the number of points in a mesh of fineness phi, covering a hypercube in d dimensions
#'
#' @param d The number of dimensions for the unit spheroid
#' @param phi The scalar fineness of the mesh
#' @return A list of: corner_pts - Count of points extreme (+/- 1) in all dim, edge_pts - Count of points extreme in all but one dimen, face_pts - Count of points extreme in all but two dimen, total_pts - Sum of:  corner_pts + edge_pts + face_pts

calc_mesh_size <- function(phi,d){
  corner_pts <-  2^d
  edges <-  d*2^(d-1)
  edge_pts <-  edges*(phi-2)
  faces <-  d*(d-1)*2^(d-3);
  face_pts <-  faces*(phi-2)*(phi-2);
  total_pts <-  corner_pts + edge_pts + face_pts
  return(list(total_pts, corner_pts, edge_pts, face_pts))
}
