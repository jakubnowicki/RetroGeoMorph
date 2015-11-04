deformacja <- function(data,strain.matrix) {
  wynik <- strain.matrix %*% t(data)
  return(t(wynik))
}