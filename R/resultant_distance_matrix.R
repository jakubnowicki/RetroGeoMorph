#' Calcualte resultant distance matrix
#' 
#' Calculate resultant distance matrix from partial distance matrices.
#' 
#' @param ... partial distance matrices
#' @param method method of calculation (sum, mean, vector)
#' @import morphoutils
#' @export

resultant.distance.matrix <- function(..., method) {
    matrices <- list(...)
    data.array <- abind.2(matrices, along = 3)
    n.matrices <- length(matrices)
    n.specimens <- dim(matrices[[1]])[1]
    pairs <- combn(1:n.specimens,2)
    n.pairs <- dim(pairs)[2]
    output <- matrix(0, ncol=n.specimens,nrow = n.specimens)
    for (i in 1:n.pairs) {
        x <- pairs[1,i]
        y <- pairs[2,i]
        tmp <- data.array[x,y,]
        output[x,y] <- resultant.distance(tmp, method = method)
        output[y,x] <- output[x,y]
    }
    return(output)
}