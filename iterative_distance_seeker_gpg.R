iterative.minimal.distance.seeker.gpg <- function(data.1, data.2,a.min=0.1,a.max=1.9,a.skok=0.1,theta.min=-0.9,theta.max=0.9,theta.skok=0.1, iteracje=10,curves=0,istotne.cyfry=10,wydruk=T) {
  dlugosc.a<-length(seq(a.min,a.max,a.skok))
  dlugosc.theta<-length(seq(theta.min,theta.max,theta.skok))
  distance<-10000
  for (i in 1:iteracje) {
    min.dist<-min.dist.seeker.gpg(data.1,data.2,a.min,a.max,a.skok,theta.min,theta.max,theta.skok,curves=curves,wydruk)
    #min.dist <- filter(min.dist,distance!='NA' && distance!='NaN' && distance != 'Inf')
    min.dist.ordered<-min.dist[order(min.dist$distance),]
    a.min<-min(head(min.dist.ordered$a))
    a.max<-max(head(min.dist.ordered$a))
    a.skok<-abs((a.max-a.min)/dlugosc.a)
    theta.min<-min(head(min.dist.ordered$theta))
    theta.max<-max(head(min.dist.ordered$theta))
    theta.skok<-abs((theta.max-theta.min)/dlugosc.theta)
    ifelse((round(distance,digits = istotne.cyfry) == round(min.dist.ordered[1,1], digits = istotne.cyfry)),break,distance<-min.dist.ordered[1,])
  }
  return(distance)
}