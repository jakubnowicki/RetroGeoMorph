#' Find minimal distance
#' 
#' Function to find minimal Procrustes distance between two specimens
#' @param data.1 First specimen
#' @param data.2 Second specimen
#' @param a.min a.min
#' @param a.max a.max
#' @param a.skok a.skok
#' @param theta.min theta.min
#' @param theta.max theta.max
#' @param theta.skok theta.skok
#' @param curves Curve matrix for semilandmarks sliding
#' @param wydruk Print distance
#' @param distance distance type ('full' or 'procrustes')
#' @export
#' @import geomorph
#' @import morphoutils

minimal.distance <- function(data.1,data.2,a.min=0.1,a.max=1.9,a.skok=0.1,theta.min=-0.9,theta.max=0.9,
                                    theta.skok=0.1,curves=NULL,wydruk=T, distance = 'full') {
  a.vector<-seq(from = a.min,to = a.max,by = a.skok)
  if (theta.skok != 0) {
    theta.vector<-seq(from = theta.min, to = theta.max, by = theta.skok)
  } else {
      theta.vector <- 0
  }
  n.kombinacji <- length(a.vector)*length(theta.vector)
  wynik<-matrix(0,ncol=3,nrow=n.kombinacji)
  n.land <- dim(data.1)[1]
  params <- expand.grid(a.vector,theta.vector)
  if (distance == 'full') {
      for (i  in 1:n.kombinacji) {
          a<-params[i,1]
          theta<-params[i,2]
          wynik[i,2]<-a
          wynik[i,3]<-theta
          strain<-strain.matrix(a,theta)
          tmp<-deformacja(data.1,strain)
          tmp.a<-array(data = c(tmp,data.2), dim = c(n.land,2,3))
          gpg <- gpagen(tmp.a,curves = curves)
          wynik[i,1]<-fpdist(gpg$coords[,,1],gpg$coords[,,2])
      }
  } else {
      if (distance == 'procrustes') {
          for (i  in 1:n.kombinacji) {
              a<-params[i,1]
              theta<-params[i,2]
              wynik[i,2]<-a
              wynik[i,3]<-theta
              strain<-strain.matrix(a,theta)
              tmp<-deformacja(data.1,strain)
              tmp.a<-array(data = c(tmp,data.2), dim = c(n.land,2,3))
              gpg <- gpagen(tmp.a,curves = curves)
              wynik[i,1]<-proc.dist(gpg$coords[,,1],gpg$coords[,,2])
          }
      } else {
          stop('wrong distance')
      }
  }
  wynik.df<-as.data.frame(wynik)
  colnames(wynik.df)<-c('distance','a','theta')
  if (wydruk == T) {
    print("Minimal distance:")
    print(min(wynik.df[,1])) }
  return(wynik.df)
}
