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
#' @param iteracje How precize should the search be
#' @param curves Curve matrix for semilandmarks sliding
#' @param istotne.cyfry Digit number in comparing distance
#' @param wydruk Print distance
#' @export

min.dist.seeker.gpg <- function(data.1,data.2,a.min=0.1,a.max=1.9,a.skok=0.1,theta.min=-0.9,theta.max=0.9,theta.skok=0.1,curves=0,wydruk=T) {
  a.vector<-seq(from = a.min,to = a.max,by = a.skok)
  theta.vector<-seq(from = theta.min, to = theta.max, by = theta.skok)
  n.kombinacji <- length(a.vector)*length(theta.vector)
  wynik<-matrix(0,ncol=3,nrow=n.kombinacji)
  for (i  in 1:n.kombinacji) {
    a<-a.vector[((i-1) %/% length(theta.vector))+1]
    theta<-theta.vector[((i-1) %% length(a.vector))+1]
    wynik[i,2]<-a
    wynik[i,3]<-theta
    strain<-strain.matrix(a,theta)
    tmp<-deformacja(data.1,strain)
    tmp.a<-abind(tmp,data.2, along = 3)
    ifelse(curves[1]==0,yes = (gpg <- gpagen(tmp.a,ShowPlot = F)), no = (gpg <- gpagen(tmp.a,curves = curves,ShowPlot = F)))
    wynik[i,1]<-fpdist(gpg$coords[,,1],gpg$coords[,,2])
  }
  wynik.df<-as.data.frame(wynik)
  colnames(wynik.df)<-c('distance','a','theta')
  if (wydruk == T) {
    print("Minimal distance:")
    print(min(wynik.df[,1])) }
  return(wynik.df)
}
