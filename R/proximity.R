#' Proximity
#'
#' @description
#' Compute the proximity \insertCite{hidalgo2007product}{ProductSpace} based on the RCA
#' values.
#'
#' @param RCAmat An economy-by-product matrix with the RCA values.
#'
#' @importFrom Rdpack reprompt
#'
#' @return A product-by-product matrix with the proximity values.
#'
#' @references
#' \insertAllCited{}
#'
#' @export

proximity <- function(RCAmat) {

  if (!is.matrix(RCAmat)) {
    stop("'RCAmat' must be an economy-by-product matrix with the RCA values!")
  }

  # Mshared / max(M.pt)
  prox <- crossprod(RCAmat, RCAmat) / outer(colSums(RCAmat), colSums(RCAmat), pmax)

  return(prox)
}
