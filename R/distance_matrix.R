#' Procrustes Distance Matrix
#' 
#' Creates Procrustes Distance matrix from landmark data.
#' 
#' @param data data
#' @param Csize optional addition of column with centroid size
#' @param distance distance type ('full' or 'procrustes')
#' @import morphoutils
#' @export

distance.matrix <- function(data, Csize=FALSE, distance = 'full') {
  landmarks <- data$coords
  if (Csize==TRUE) size<-data$Csize
  n<-dim(landmarks)[3]
  wynik<-matrix(0,n,n)
  if (distance == 'full') {
      for (i in 1:n) {
          for (j in 1:n) {
              wynik[i,j]<-fpdist(x=landmarks[,,i],y = landmarks[,,j])
          }
      }
  } else {
      if (distance == 'procrustes') {
          for (i in 1:n) {
              for (j in 1:n) {
                  wynik[i,j]<-morphoutils::proc.dist(x=landmarks[,,i],y = landmarks[,,j])
              }
          }
      } else {
          stop('wrong distance')
      }
  }
  wynik.df<-as.data.frame(wynik)
  colnames(wynik.df)<-attr(landmarks,which = 'dimnames')[[3]]
  rownames(wynik.df)<-attr(landmarks,which = 'dimnames')[[3]]
  if (Csize==TRUE) wynik.df$size<-size
  return(wynik.df)
}