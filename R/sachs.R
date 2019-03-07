#' Synthetic data based off Sachs(2005)
#'
#' Synthetic data generated from the protein signaling network found in
#' "Causal Protein-Signaling Networks Derived from Multiparameter Single-Cell Data"
#' @format Two R objects:
#' \describe{
#'   \item{sachs.df}{A data frame with 500 rows and 11 variables, each with
#'   three levels: LOW, AVG, HIGH. The variables are proteins and values
#'   represent the concentration.}
#'   \item{sachs.dag}{A causality.dag; it is the learned graph from the paper.}
#' }
#' @references
#' Sachs K, Perez O, Pe'er D, Lauffenburger DA, Nolan GP. Causal
#' protein-signaling networks derived from multiparameter single-cell data.
#' Science. 2005 Apr 22;308(5721):523-9.
#' @source The dataframe was generated from the R package \code{bnlearn}
#' @name sachs
#' @aliases NULL
NULL

#' @rdname sachs
"sachs.df"

#' @rdname sachs
"sachs.dag"