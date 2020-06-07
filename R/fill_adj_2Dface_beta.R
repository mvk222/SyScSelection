#' Calculates the factor, beta in [0, 1], that interpolates the pth equidistant point between the two endpoints, z_one and z_phi, for and adjusted 2D mesh of fineness phi in d dimensions.
#'
#' @param p ...
#' @param phi Fineness of the mesh along each dimension of the 2D face
#' @param z_one ...
#' @param z_phi ...
#' @return beta
#' @import pracma

fill_adj_2Dface_beta <- function(p, phi, z_one, z_phi){
  n_one <- norm(z_one,type="2")
  n_phi <- norm(z_phi,type="2")
  zTone <- z_one%*%t(z_one)
  zTphi <- z_phi%*%t(z_phi)
  zToxp <- z_one%*%t(z_phi)
  theta <- acos(zToxp/(n_one*n_phi))/(phi-1)
  g_one <- n_one*cos((p-1)*theta)
  g_phi <- n_phi*cos((phi-p)*theta)
  beta <- zTphi*g_one-zToxp*g_phi
  beta <- mrdivide(beta,((zTphi-zToxp)*g_one+(zTone-zToxp)*g_phi))
  beta <- mean(beta)
  if (abs(beta < 0.00000000000001)){
    beta <- 0
  }
  return(beta)
}
