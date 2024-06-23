#'
#' Ubiquity
#'
#' @description
#' Compute the ubiquity \insertCite{hidalgo2009dynamics}{ProductSpace} based on RCA values.
#'
#' @param RCAmat An economy-by-product matrix with the RCA values.
#'
#' @importFrom Rdpack reprompt
#'
#' @return A numeric vectors.
#'
#' @references
#' \insertAllCited{}

ubiquity <- function(RCAmat) {

  if (!is.matrix(RCAmat)) {
    stop("'RCAmat' must be an economy-by-product matrix with the RCA values!")
  }

  result <- colSums(RCAmat)

  return(result)
}
