#' Deform landmark data
#' 
#' Deforms landmark data using strain matrix.
#' 
#' @param data Landmark data (single specimens matrix)
#' @param strain.matrix Strain matrix

deformacja <- function(data,strain.matrix) {
  wynik <- strain.matrix %*% t(data)
  return(t(wynik))
}