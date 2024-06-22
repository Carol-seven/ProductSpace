#'
#' Revealed Comparative Advantage Index / Balassa Index
#'
#' @description
#' Computes the revealed comparative advantage (RCA) index
#' \insertCite{balassa1989revealed}{ProductSpace} for a given dataset.
#'
#' @param data A data frame or matrix containing the data.
#' \itemize{
#' \item If a data frame, it should have columns representing economies, products, and
#' export values.
#' \item If a matrix, it should be an economy-by-product matrix with export values.
#' }
#'
#' @param econ A character string (default = "economy") specifying the column name for
#' economies when \code{data} is a data frame.
#'
#' @param prod A character string (default = "product") specifying the column name for
#' products when \code{data} is a data frame.
#'
#' @param exp A character string (default = "export") specifying the column name for
#' export values when \code{data} is a data frame.
#'
#' @param binary A boolean (default = FALSE) specifying whether to return binary RCA
#' values (1 if RCA >= threshold, 0 otherwise).
#'
#' @param threshold A numeric value (default = 1) specifying the threshold for binary RCA
#' values.
#'
#' @import tidyr
#' @importFrom Rdpack reprompt
#' @importFrom tibble column_to_rownames
#'
#' @return An economy-by-product matrix with the RCA values.
#'
#' @references
#' \insertAllCited{}

RCA <- function(data, binary = FALSE, threshold = 1,
                econ = "economy", prod = "product", exp = "export") {

  if (!(is.data.frame(data) | is.matrix(data))) {
    stop("'data' must be a data.frame or economy-by-product matrix!")
  }

  if (!is.character(econ) | !is.character(prod) | !is.character(exp)) {
    stop("'econ', 'prod', and 'value' must be character strings!")
  }

  if (!is.logical(binary)) {
    stop("'binary' must be boolean!")
  }

  if (!is.numeric(threshold)) {
    stop("'threshold' must be numeric!")
  }

  if (is.data.frame(data)) {
    data <- data %>%
      spread(prod, exp, fill = 0) %>%
      column_to_rownames(var = econ)
  }

  ## RCAep = (Xep / Xe.) / (X.p / X..)
  RCA <- t(t(data / rowSums(data)) / (colSums(data) / sum(data)))

  if (binary) {
    RCA[RCA < threshold] <- 0
    RCA[RCA >= threshold] <- 1
  }

  return(RCA)
}

