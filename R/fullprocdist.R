#' Procrustes distance
#' 
#' Finds Procrustes distance between two objects
#' @param x Object 1
#' @param y Object 2
#' @param reflect reflect
#' @export
#' @import shapes

fpdist<-function(x, y, reflect = FALSE){
  sin(riemdist(x,y,reflect=reflect))
}