#' Create minimal distance matrix
#' 
#' Function for generating minimal Procrustes distance matrix from tectonicaly deformed landmark dataset.
#' @param data Landmark dataset
#' @param Csize Optional column with centroid size. Defaults to FALSE
#' @param curve Optional matrix for sliding semilandmarks
#' @param a.min a.min
#' @param a.max a.max
#' @param a.skok a.skok
#' @param theta.min theta.min
#' @param theta.max theta.max
#' @param theta.skok theta.skok
#' @param iteracje How precize should the search be
#' @param istotne.cyfry Digit number in comparing distance
#' @export
#' @import doParallel

minimal.distance.matrix <- function(data, Csize=FALSE,curve=NULL,a.min=0.1,a.max=1.9,a.skok=0.1,theta.min=-0.9,
                                              theta.max=0.9,theta.skok=0.1, iteracje=10,istotne.cyfry=10) {
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl, cores = detectCores() - 1)
  landmarks <- data$coords
  if (Csize==TRUE) size<-data$Csize
  n<-dim(landmarks)[3]
  pairs<-combn(1:n,2)
  n.pairs<-dim(pairs)[2]
  wynik<-NULL
  wynik =  foreach(i=1:n.pairs,.export = c('iterative.minimal.distance','minimal.distance'),
                   .packages = c('shapes','geomorph','dplyr','morphoutils')) %dopar% {
    wynik[i]<-iterative.minimal.distance.seeker.gpg(data.1=landmarks[,,pairs[1,i]],data.2=landmarks[,,pairs[2,i]],
                                                    curves = curve,wydruk=F,a.min=a.min,a.max=a.max,a.skok=a.skok,
                                                    theta.min=theta.min,theta.max=theta.max,theta.skok=theta.skok,
                                                    istotne.cyfry=istotne.cyfry,iteracje=iteracje)[,1]
  }
  stopCluster(cl)
  wynik<-unlist(wynik)
  wynik.m <- matrix(0,ncol=n,nrow=n)
  for (i in 1:n.pairs) {
    wynik.m[pairs[1,i],pairs[2,i]]<-wynik[i]
    wynik.m[pairs[2,i],pairs[1,i]]<-wynik[i]
  }
  wynik.df<-as.data.frame(wynik.m)
  colnames(wynik.df)<-attr(landmarks,which = 'dimnames')[[3]]
  rownames(wynik.df)<-attr(landmarks,which = 'dimnames')[[3]]
  if (Csize==TRUE) wynik.df$size<-size
  return(wynik.df)
}