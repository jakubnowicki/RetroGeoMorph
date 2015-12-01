#' Create minimal distance matrix
#' 
#' Function for generating minimal Procrustes distance matrix from tectonicaly deformed landmark dataset.
#' @param data Landmark dataset
#' @param Csize Optional column with centroid size. Defaults to FALSE
#' @param curve Optional matrix for sliding semilandmarks
#' @export
#' @import doParallel

minimal.distance.matrix.iter.half <- function(data, Csize=FALSE,curve=0) {
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl, cores = detectCores() - 1)
  landmarks <- data$coords
  if (Csize==TRUE) size<-data$Csize
  n<-dim(landmarks)[3]
  pairs<-combn(c(1:n),2)
  n.pairs<-dim(pairs)[2]
  wynik<-NULL
  wynik =  foreach(i=1:n.pairs,.export = c('fpdist','iterative.minimal.distance.seeker.gpg','min.dist.seeker.gpg','strain.matrix','deformacja'),.packages = c('shapes','abind','geomorph','dplyr')) %dopar% {
    wynik[i]<-iterative.minimal.distance.seeker.gpg(data.1=landmarks[,,pairs[1,i]],data.2=landmarks[,,pairs[2,i]],curves = curve,wydruk=F)[,1]
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