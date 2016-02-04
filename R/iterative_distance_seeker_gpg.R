#' Find minimal distance
#' 
#' Function to find minimal Procrustes distance between two specimens in iterative way with increasing degree of accuracy
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
#' @param distance distance type ('full' or 'procrustes')
#' @export

iterative.minimal.distance <- function(data.1, data.2,a.min=0.1,a.max=1.9,a.skok=0.1,theta.min=-0.9,
                                                  theta.max=0.9,theta.skok=0.1, iteracje=10,curves=NULL,
                                                  istotne.cyfry=10,wydruk=T, dist = 'full') {
    dlugosc.a<-length(seq(a.min,a.max,a.skok))
    if (theta.skok != 0) {
        dlugosc.theta<-length(seq(theta.min,theta.max,theta.skok))
    } else {
        dlugosc.theta <- 0
        theta.min <- 0
    }
    distance<-10000
    if (theta.skok != 0) {
        for (i in 1:iteracje) {
            min.dist<-minimal.distance(data.1,data.2,a.min,a.max,a.skok,theta.min,theta.max,theta.skok,
                                              curves=curves,wydruk, distance = dist)
            min.dist.ordered<-min.dist[order(min.dist$distance),]
            ifelse((round(distance,digits = istotne.cyfry) == round(min.dist.ordered[1,1], digits = istotne.cyfry)),
                   break,distance<-min.dist.ordered[1,])
            a.min<-min(head(min.dist.ordered$a))
            a.max<-max(head(min.dist.ordered$a))
            a.skok<-abs((a.max-a.min)/dlugosc.a)
            theta.min<-min(head(min.dist.ordered$theta))
            theta.max<-max(head(min.dist.ordered$theta))
            theta.skok<-abs((theta.max-theta.min)/dlugosc.theta)
        }
    } else {
        for (i in 1:iteracje) {
            min.dist<-minimal.distance(data.1,data.2,a.min,a.max,a.skok,theta.min,theta.max,theta.skok,
                                              curves=curves,wydruk, distance = dist)
            min.dist.ordered<-min.dist[order(min.dist$distance),]
            ifelse((round(distance,digits = istotne.cyfry) == round(min.dist.ordered[1,1], digits = istotne.cyfry)),break,
                   distance<-min.dist.ordered[1,])
            a.min<-min(head(min.dist.ordered$a))
            a.max<-max(head(min.dist.ordered$a))
            a.skok<-abs((a.max-a.min)/dlugosc.a)
        }
    }
    return(distance)
}