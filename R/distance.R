#'
#' Distance
#'
#' @description
#' Compute the distance \insertCite{hausmann2006structural}{ProductSpace} based on the RCA
#' and proximity values.
#'
#' @param RCAmat An economy-by-product matrix with the RCA values.
#'
#' @param PROXmat A product-by-product matrix with the proximity values.
#'
#' @importFrom Rdpack reprompt
#'
#' @return A economy-by-product matrix with the distance values.
#'
#' @references
#' \insertAllCited{}

distance <- function(RCAmat, PROXmat) {

  if (!is.matrix(RCAmat)) {
    stop("'RCAmat' must be an economy-by-product matrix with the RCA values!")
  }

  if (!is.matrix(PROXmat)) {
    stop("'PROXmat' must be an product-by-product matrix with the proximity values!")
  }

  distance <- (1 - RCAmat) %*% t(PROXmat / rowSums(PROXmat))

  return(distance)
}
