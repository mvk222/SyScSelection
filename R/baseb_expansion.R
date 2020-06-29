#' Adds the next base-b element to an existing base-b sequence
#' @param ain Either a an array containing an existing base-b expansion, or a scalar integer indicating the length for a new base-b expansion
#' @param b Base for integer expansions used in the sequence
#' @return An expanded base-b expansion

baseb_expansion <- function(ain, b) {
  if (length(ain) == 1) {
    aout <-  new_baseb_expansion(ain, b)
  } else {
    m <-  length(ain)
    carry <- TRUE
    aout <-  matrix(0, 1, m + 1)
    for (i in m:1) {
      if (carry) {
        if (ain[i] == (b - 1)) {
          aout[i] <-  0
        } else {
          aout[i] = ain[i] + 1
          carry = FALSE
        }
      } else {
        aout[i] <- ain[i]
      }
    }
    if (carry) {
      aout[1] <-  1
      aout[2:(m + 1)] <-  as.vector(aout)
    }
  }
  return(aout)
}