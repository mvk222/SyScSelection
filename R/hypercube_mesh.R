#' Generates a Cartesian mesh of d-dimensional scenarios based on the given ellipsoid. This function does not assume that the ellipsoid is centered at the origin.
#'
#' @param phi The scalar fineness of the mesh
#' @param hellip The basis for the shocks; it must have measurable width in every dimension
#' @param normalize Whether to normalize points from the cube onto the sphere or not (TRUE/FALSE)
#' @return A d x N array, with each column a scenario
#' @import pracma
#' @examples 
#' hellip <- hyperellipsoid()
#' hypercube_mesh(3,hellip,TRUE)
#' @export

hypercube_mesh <- function(phi,hellip, normalize){
  # Transform the ellipsoid to the centered unit spheroid:
  centerlist <- center_at_origin(hellip)
  hellip3 <- centerlist[[1]]
  mu <- centerlist[[2]]
  d <- length(mu)
  rotationlist <- rotate_to_coordaxes(hellip3)
  hellip2 <- rotationlist[[1]]
  tfm_rot <- rotationlist[[2]]
  stretchlist <- stretch_to_unitspheroid(hellip2)
  hellip1 <- stretchlist[[1]]
  tfm_str <- stretchlist[[2]]
  # The basic Cartesian mesh:
  mesh <- spheroid_mesh(d, phi, normalize)
  mesh_size <- length(mesh[1,])
  # Transform (to an ellipsoid) and recenter the shocks:
  mesh <- mldivide(tfm_str,mesh)
  mesh <- mldivide(tfm_rot,mesh)
  mesh <-  mesh + mu %*% matrix(1, 1, mesh_size)
  return(mesh)
}
