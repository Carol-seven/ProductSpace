% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rca.R
\name{rca}
\alias{rca}
\title{Revealed Comparative Advantage Index / Balassa Index}
\usage{
rca(
  data,
  binary = TRUE,
  threshold = 1,
  econ = "economy",
  prod = "product",
  exp = "export"
)
}
\arguments{
\item{data}{A data frame or matrix containing the data.
\itemize{
\item If a data frame, it should have columns representing economies, products, and
export values.
\item If a matrix, it should be an economy-by-product matrix with export values.
}}

\item{binary}{A boolean (default = TRUE) specifying whether to return binary RCA values
(1 if RCA >= threshold, 0 otherwise).}

\item{threshold}{A numeric value (default = 1) specifying the threshold for binary RCA
values.}

\item{econ}{A character string (default = "economy") specifying the column name for
economies when \code{data} is a data frame.}

\item{prod}{A character string (default = "product") specifying the column name for
products when \code{data} is a data frame.}

\item{exp}{A character string (default = "export") specifying the column name for
export values when \code{data} is a data frame.}
}
\value{
An economy-by-product matrix with the RCA values.
}
\description{
Compute the revealed comparative advantage (RCA) index
\insertCite{balassa1965trade}{ProductSpace} for a given dataset.
}
\references{
\insertAllCited{}
}
