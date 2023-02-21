#' Functions for converting between effect sizes
#'
#' @name conversions
#' @author Taren Sanders
NULL


#' @describeIn conversions Convert beta to r
b2r <- function(beta) {
  # conversion formula from 10.1007/s11162-011-9232-5
  b2r_single <- function(beta) {
    ifelse(!is.na(beta), beta, NA_real_)
  }

  vapply(beta, b2r_single, numeric(1))
}
