#'
#' Complexity Index
#'
#' @description
#' Compute the economic complexity index (ECI) and the product complexity index (PCI)
#' based on the RCA values.
#'
#' @param RCAmat An economy-by-product matrix with the RCA values.
#'
#' @param method A character string specifying the method to calculate: \itemize{
#' \item "eigen": the eigenvector solution \insertCite{hausmann2014atlas}{ProductSpace}.
#' \item "fc": fitness-complexity method
#' \insertCite{tacchella2012new,pugliese2016convergence}{ProductSpace}.
#' \item "mr": method of reflections
#' \insertCite{hidalgo2009building,caldarelli2012network}{ProductSpace}.
#' }
#'
#' @param niter An integer (default = 20) specifying the number of iterations for
#' \code{method = "fc"} and \code{method = "mr"}.
#'
#' @param gamma A numeric value (default = 1) specifying the tuning parameter for
#' \code{method = "fc"}.
#'
#' @importFrom stats cor setNames
#' @importFrom Rdpack reprompt
#'
#' @return A list containing ECI and PCI.
#'
#' @references
#' \insertAllCited{}
#'
#' @export

complexity <- function(RCAmat, method = c("eigen", "fc", "mr"), niter = 20, gamma = 1) {

  if (!is.matrix(RCAmat)) {
    stop("'RCAmat' must be an economy-by-product matrix with the RCA values!")
  }

  method <- match.arg(method)

  if (method == "eigen") {

    ## Mee' =  \frac{1}{Ke0} \sum_p Mep * (Me'p / Kp0)
    ## M <- RCAmat %*% (t(RCAmat) / colSums(RCAmat)) / rowSums(RCAmat)
    M <- t(RCAmat) / colSums(RCAmat)
    M[is.nan(M)] <- 0
    M <- RCAmat %*% M / rowSums(RCAmat)
    M[is.nan(M)] <- 0
    eci <- scale(Re(eigen(M)$vectors[,2]), center = TRUE, scale = TRUE)

    ## Mpp' = \frac{1}{Kp0} \sum_e Mep * (Mep' / Ke0)
    ## M <- t(RCAmat) %*% (RCAmat / rowSums(RCAmat)) / colSums(RCAmat)
    M <- RCAmat / rowSums(RCAmat)
    M[is.nan(M)] <- 0
    M <- t(RCAmat) %*% M / colSums(RCAmat)
    M[is.nan(M)] <- 0
    pci <- scale(Re(eigen(M)$vectors[,2]), center = TRUE, scale = TRUE)

    # if (cor(eci, rowSums(RCAmat), use = "pairwise.complete.obs") < 0) {
    #   eci <- -eci
    #   pci <- -pci
    # }

  } else if (method == "fc") {

    eci <- matrix(NA, nrow(RCAmat), niter)
    eci[,1] <- 1

    pci <- matrix(NA, ncol(RCAmat), niter)
    pci[,1] <- 1

    for (k in 2:niter) {
      eci[,k] <- RCAmat %*% pci[,k-1]
      eci[,k] <- eci[,k] / mean(eci[,k], na.rm = TRUE)
      ## pci[,k] <- (t(RCAmat) %*% eci[,k-1]^(-gamma))^(-1/gamma)
      temp <- eci[,k-1]^(-gamma)
      temp[is.infinite(temp)] <- 0
      pci[,k] <- (t(RCAmat) %*% temp)^(-1/gamma)
      pci[,k][is.infinite(pci[,k])] <- 0
      pci[,k] <- pci[,k] / mean(pci[,k], na.rm = TRUE)
    }

    eci <- eci[,niter]
    pci <- pci[,niter]

  } else if (method == "mr") {

    eci <- matrix(NA, nrow(RCAmat), niter)
    eci[,1] <- rowSums(RCAmat) ## k_{e,0}

    pci <- matrix(NA, ncol(RCAmat), niter)
    pci[,1] <- colSums(RCAmat) ##k_{p,0}

    for (k in 2:niter) {
      ## k_{e,k-1}
      eci[,k] <- (RCAmat %*% pci[,k-1]) / rowSums(RCAmat)
      eci[,k][is.nan(eci[,k]) | is.infinite(eci[,k])] <- 0
      ## k_{p,k-1}
      pci[,k] <- (t(RCAmat) %*% eci[,k-1]) / colSums(RCAmat)
      pci[,k][is.nan(pci[,k]) | is.infinite(pci[,k])] <- 0
    }

    ## even-numbered iterations ke0, ke2, ke4, ... indicate the diversification of economics
    ## odd-numbered iterations ke1, ke3, ke5, ... indicate the ubiquity of products.
    ## even-numbered iterations kp0, kp2, kp4, ... indicate the product's ubiquity
    ## odd-numbered iterations kp1, kp3, kp5, ... indicate the diversification of the exporters of the product

    ## eci: odd-numbered columns (even-numbered iterations ke0, ke2, ke4, ...)
    ## pci: even-numbered columns (odd-numbered iterations kp1, kp3, kp5, ...)

    eci <- scale(eci[,ifelse(niter %% 2 == 0, niter - 1, niter)], center = TRUE, scale = TRUE)
    pci <- scale(pci[,ifelse(niter %% 2 == 0, niter, niter - 1)], center = TRUE, scale = TRUE)
  }

  result <- list(eci = setNames(as.numeric(eci), rownames(RCAmat)),
                 pci = setNames(as.numeric(pci), colnames(RCAmat)))
  return(result)
}
