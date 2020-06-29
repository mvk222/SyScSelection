#' Show basic facts of a hyperellipsoid (center, shape, size)
#'
#' @param hellip A valid hyperellipsoid object
#' @return none
#' @examples display(hellip)
#' @export

display <- function(hellip){
  message("center:")
  print(hellip$center)
  message("shape:")
  print(hellip$shape)
  message("size:")
  print(hellip$size)
}
