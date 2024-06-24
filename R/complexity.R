#'
#' Complexity Index
#'
#' @description
#' Compute the economic complexity index (ECI) and the product complexity index (PCI)
#' \insertCite{hidalgo2009building}{ProductSpace} based on the RCA values.
#'
#' @param RCAmat An economy-by-product matrix with the RCA values.
#'
#' @param measure A character string specifying the measure to compute:
#' \enumerate{
#' \item "eci": economic complexity index.
#' \item "pci": product complexity index.
#' }
#'
#' @importFrom stats setNames
#' @importFrom Rdpack reprompt
#'
#' @return A numeric vector with the measured values.
#'
#' @references
#' \insertAllCited{}
#'
#' @export

complexity <- function(RCAmat, measure = c("eci", "pci")) {

  if (!is.matrix(RCAmat)) {
    stop("'RCAmat' must be an economy-by-product matrix with the RCA values!")
  }

  measure <- match.arg(measure)

  if (measure == "eci") {

    # Mee' =  \frac{1}{Ke0} \sum_p Mep * (Me'p / Kp0)
    # M <- RCAmat %*% (t(RCAmat) / colSums(RCAmat)) / rowSums(RCAmat)
    M <- t(RCAmat) / colSums(RCAmat)
    M[is.nan(M)] <- 0
    M <- RCAmat %*% M / rowSums(RCAmat)
    M[is.nan(M)] <- 0
    eci <- scale(Re(eigen(M)$vectors[,2]), center = TRUE, scale = TRUE)
    result <- setNames(as.numeric(eci), rownames(RCAmat))

  } else if (measure == "pci") {

    # Mpp' = \frac{1}{Kp0} \sum_e Mep * (Mep' / Ke0)
    # M <- t(RCAmat) %*% (RCAmat / rowSums(RCAmat)) / colSums(RCAmat)
    M <- RCAmat / rowSums(RCAmat)
    M[is.nan(M)] <- 0
    M <- t(RCAmat) %*% M / colSums(RCAmat)
    M[is.nan(M)] <- 0
    pci <- scale(Re(eigen(M)$vectors[,2]), center = TRUE, scale = TRUE)
    result <- setNames(as.numeric(pci), colnames(RCAmat))
  }

  return(result)
}
