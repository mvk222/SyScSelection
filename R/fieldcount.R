#' Utility function for number of fields in a hyperellipsoid
#'
#' @param hellip A valid hyperellipsoid object
#' @return The number of fields in the object (should be 3)
#' @export

fieldcount <- function(hellip){
  numFields <- length(names(hellip))
  return(numFields)
}
