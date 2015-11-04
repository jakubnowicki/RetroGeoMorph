strain.deformation <-function(data,a,theta) {
  strain<-matrix(0,ncol=2,nrow=2)
  strain[1,1]<-a*cos(theta)
  strain[1,2]<-sin(theta)
  strain[2,1]<-sin(theta)
  strain[2,2]<-(1/a)*cos(theta)
  wynik <- strain %*% t(data)
  return(t(wynik))
}
strain.deformation.comp<-cmpfun(f = strain.deformation)
