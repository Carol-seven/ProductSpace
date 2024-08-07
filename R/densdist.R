#'
#' Density and Distance
#'
#' @description
#' Compute the density and distance \insertCite{hausmann2006structural}{ProductSpace}
#' based on the RCA and proximity values.
#'
#' @param RCAmat An economy-by-product matrix with the RCA values.
#'
#' @param PROXmat A product-by-product matrix with the proximity values.
#'
#' @param measure A character string specifying the measure to compute:
#' \enumerate{
#' \item "density": the new product’s proximity to the economy's current export basket.
#' \item "distance": the economy's ability to enter a specific product.
#' }
#'
#' @importFrom Rdpack reprompt
#'
#' @return A economy-by-product matrix with the measured values.
#'
#' @references
#' \insertAllCited{}
#'
#' @export

densdist <- function(RCAmat, PROXmat, measure = c("density", "distance")) {

  if (!is.matrix(RCAmat)) {
    stop("'RCAmat' must be an economy-by-product matrix with the RCA values!")
  }

  if (!is.matrix(PROXmat)) {
    stop("'PROXmat' must be an product-by-product matrix with the proximity values!")
  }

  measure <- match.arg(measure)

  result <- t(PROXmat / rowSums(PROXmat))
  result[is.nan(result)] <- 0

  if (measure == "density") {
    ## result <- RCAmat %*% t(PROXmat / rowSums(PROXmat))
    result <- RCAmat %*% result
  } else if (measure == "distance") {
    ## result <- (1 - RCAmat) %*% t(PROXmat / rowSums(PROXmat))
    result <- (1 - RCAmat) %*% result
  }

  return(result)
}
