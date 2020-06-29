#' Hyperellipsoid class constructor
#' @param ... mu - The vector for the center point,
#' sig - The matrix determining the shape; for elliptical probability distributions, sig will be the inverse dispersion matrix,
#' c - The scalar determining the size
#' @return A new hyperellipsoid object
#' @examples hyperellipsoid()
#' @export

hyperellipsoid <- function(...){
  varargin <- list(...)
  nargin <- length(varargin)
  if (nargin == 0){
    # if no input arguments, create a 3-D sphere
    hellip <- list()
    hellip$center <- matrix(0, 3, 1)
    hellip$shape <- diag(3)
    hellip$size <- 1
    class(hellip) <- "hyperellipsoid"
  }
  if (nargin == 1){
    # if single argument of class hyperellipsoid, return it
    if (class(varargin[[1]])=="hyperellipsoid"){
      hellip <- varargin[[1]]
    } else {
      stop("Input argument is not a hyperellipsoid object.")
    }
  }
  if (nargin == 3){
    # check if input dimensions are correct
    d <- dim(as.array(varargin[[1]]))[1]
    Ccent <- dim(as.array(varargin[[1]]))[2]
    if (!is.na(Ccent) & Ccent != 1){
      #message("hellip$center:")
      #print(varargin[[1]])
      stop("Wrong number of colums in center vector")
    }
    Rshap <- dim(as.array(varargin[[2]]))[1]
    Cshap <- dim(as.array(varargin[[2]]))[2]
    if (Rshap != d){
      #message("hellip$shape:")
      #print(varargin[[2]])
      stop("Wrong number of rows in shape matrix.")
    }
    if (Cshap != d){
      #message("hellip$center:")
      #print(varargin[[1]])
      #message("hellip$shape:")
      #print(varargin[[2]])
      stop("Wrong number of columns in shape matrix.")
    }
    Rsize <- dim(as.array(varargin[[3]]))[1]
    Csize <- dim(as.array(varargin[[3]]))[2]
    if (Rsize != 1 | (!is.na(Csize) & Csize !=1)){
      #message("hellip$size:")
      #print(varargin[[3]])
      stop("Wrong dimension of size scalar.")
    }
    # create object using specified values
    hellip <- list()
    hellip$center <- varargin[[1]]
    hellip$shape <- varargin[[2]]
    hellip$size <- varargin[[3]]
    if (hellip$size < 0){
      stop("Negative size parameter not allowed.")
    }
    eig <- eigen(solve(hellip$shape))$values
    d <- length(eig)
    for (i in 1:d){
      # See Strang, p. 250
      if (eig[i]<=0){
        stop("Shape matrix is not positive definite.")
      }
    }
    class(hellip) <- "hyperellipsoid"
  }
  if (nargin != 0 & nargin != 1 & nargin !=3){
    # Cannot cope with this number of inputs
    stop("Wrong number of input arguments.")
  }
  return(hellip)
}
