#' Get hyperellipsoid property from the specified object and return the value.
#' Property names are:
#' center, shape, and size
#'
#' @param hellip A valid hyperellipsoid object
#' @param propName A string of the desired property
#' @return The value of the indicated property

get <- function(hellip,propName){
  if (propName == "center"){
    val <- hellip$center
  }
  if (propName == "shape"){
    val <- hellip$shape
  }
  if (propName == "size"){
    val <- hellip$size
  }
  if (propName != "center" & propName != "shape" & propName != "size"){
    stop("No valid hyperellipsoid property.")
  }
  return(val)
}
