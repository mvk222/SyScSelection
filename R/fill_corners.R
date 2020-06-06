#' Systematically fills a given mesh array (cmesh) with d-dimensional  points representing every corner of a d-dimensional hypercube. The function fills the successive dimensions of each point via depth-first recursion across all d dimensions.
#'
#' @param cmesh The mesh to be filled with corner points
#' @param shock The current shock vector being filled
#' @param shk_curs Index in cmesh of the shock currently being filled
#' @param dim_curs Index in the current shock of the dimension being filled
#' @return A list of:  cmesh - d x 2^d array of corner points being filled, shk_curs - last point in cmesh that was filled

fill_corners <- function(cmesh, shock, shk_curs, dim_curs){
  d <- max(dim(shock))
  for (i in c(1,-1)){
    shock[dim_curs] <-  i
    if (dim_curs < d){
      # Recurse through all d dimensions:
      cornerlist <- fill_corners(cmesh, shock, shk_curs, dim_curs+1)
      cmesh <- cornerlist[[1]]
      shk_curs <- cornerlist[[2]]
    } else {
      shk_curs <- shk_curs + 1
      cmesh[,shk_curs] <- t(shock)
    }
  }
  return(list(cmesh, shk_curs))
}
