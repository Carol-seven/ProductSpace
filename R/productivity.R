#'
#' PRODY and EXPY
#'
#' @description
#' Compute the PRODY and EXPY \insertCite{hausmann2007you}{ProductSpace} based on export
#' and gross domestic product (GDP) data.
#'
#' @param expData A data frame or matrix containing the export data.
#' \itemize{
#' \item If a data frame, it should have columns representing economies, products, and
#' export values.
#' \item If a matrix, it should be an economy-by-product matrix with export values.
#' }
#'
#' @param gdpData A data frame containing the GDP data.
#'
#' @param measure A character string specifying the measure to compute:
#' \enumerate{
#' \item "prody": the income/productivity level.
#' \item "expy": the income/productivity level of a country's export basket.
#' }
#'
#' @param econ A character string (default = "economy") specifying the column name for
#' economies when \code{expData} is a data frame.
#'
#' @param prod A character string (default = "product") specifying the column name for
#' products when \code{expData} is a data frame.
#'
#' @param exp A character string (default = "export") specifying the column name for
#' export values when \code{expData} is a data frame.
#'
#' @param gdp A character string (default = "GDP") specifying the column name for
#' GDP values when \code{gdpData} is a data frame.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats setNames
#' @importFrom tibble column_to_rownames
#' @importFrom Rdpack reprompt
#'
#' @return A numeric vector with the measured values.
#'
#' @references
#' \insertAllCited{}

productivity <- function(expData, gdpData,
                         measure = c("prody", "expy"),
                         econ = "economy", prod = "product", exp = "export", gdp = "GDP") {

  if (!(is.data.frame(expData) | is.matrix(expData))) {
    stop("'expData' must be a data.frame or economy-by-product matrix!")
  }

  if (!(is.data.frame(gdpData) | is.numeric(gdpData))) {
    stop("'gdpData' must be a data.frame!")
  }

  if (is.data.frame(expData)) {
    expData <- expData %>%
      select(econ, prod, exp) %>%
      spread(prod, exp, fill = 0) %>%
      column_to_rownames(var = econ) %>%
      as.matrix.data.frame()
  }

  if (is.data.frame(gdpData)) {
    gdpData <- gdpData[[gdp]]
  }

  # numerator = Xep / Xe.
  numerator <- expData / rowSums(expData)
  prody <- (t(numerator) / colSums(numerator)) %*% gdpData

  measure <- match.arg(measure)

  if (measure == "prody") {
    result <- setNames(as.numeric(prody), colnames(expData))
  } else if (measure == "expy") {
    result <- setNames(as.numeric(numerator %*% prody), rownames(expData))
  }

  return(result)
}
