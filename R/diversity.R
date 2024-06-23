#'
#' Diversity
#'
#' @description
#' Compute the diversity/diversification \insertCite{hidalgo2009dynamics}{ProductSpace}
#' based on RCA values.
#'
#' @param RCAmat An economy-by-product matrix with the RCA values.
#'
#' @importFrom Rdpack reprompt
#'
#' @return A numeric vectors.
#'
#' @references
#' \insertAllCited{}

diversity <- function(RCAmat) {

  if (!is.matrix(RCAmat)) {
    stop("'RCAmat' must be an economy-by-product matrix with the RCA values!")
  }

  result <- rowSums(RCAmat)

  return(result)
}
