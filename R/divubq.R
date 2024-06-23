#'
#' Diversity and Ubiquity
#'
#' @description
#' Compute the diversity/diversification and ubiquity
#' \insertCite{hidalgo2009dynamics}{ProductSpace} based on the RCA values.
#'
#' @param RCAmat An economy-by-product matrix with the RCA values.
#'
#' @param measure A character string specifying the measure to compute:
#' \enumerate{
#' \item "diversity": the number of products that an economy produces.
#' \item "ubiquity": the number of economies that export that product.
#' }
#'
#' @importFrom Rdpack reprompt
#'
#' @return A numeric vector with the measured values.
#'
#' @references
#' \insertAllCited{}
#'
#' @export

divubq <- function(RCAmat, measure = c("diversity", "ubiquity")) {

  if (!is.matrix(RCAmat)) {
    stop("'RCAmat' must be an economy-by-product matrix with the RCA values!")
  }

  measure <- match.arg(measure)

  if (measure == "diversity") {
    result <- rowSums(RCAmat)
  } else if (measure == "ubiquity") {
    result <- colSums(RCAmat)
  }

  return(result)
}
