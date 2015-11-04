#' Generate strain matrix
#' 
#' Generates strain matrix
#' @param a a
#' @param theta theta
#' @export

strain.matrix <- function(a, theta) {
  wynik<-matrix(0,ncol=2,nrow=2)
  wynik[1,1]<-a*cos(theta)
  wynik[1,2]<-sin(theta)
  wynik[2,1]<-sin(theta)
  wynik[2,2]<-(1/a)*cos(theta)
  return(wynik)
}
#strain.matrix.comp<-cmpfun(f = strain.matrix)
