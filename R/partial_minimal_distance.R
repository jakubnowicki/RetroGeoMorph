#' Calculate procrustes distance between parts
#' 
#' Calculate procrustes distance between parts of objects
#' 
#' @param data landmark array
#' @param list.of.elements list of elements
#' @param a a matrix
#' @param theta theta matrix
#' @param distance distance type ('full' or 'procrustes')
#' @param curves Curve matrix for semilandmarks sliding
#' @import morphoutils
#' @import geomorph
#' @export

partial.minimal.distance <- function(data, list.of.elements, a, theta, distance = 'full', curves = NULL) {
    n.elements <- length(list.of.elements)
    n.specimens <- dim(data)[3]
    pairs<-combn(1:n.specimens,2)
    n.pairs<-dim(pairs)[2]
    output <- list()
    if (distance == 'full') {
        for (i in 1:n.elements) {
            tmp.element <- matrix(0,ncol=n.specimens,nrow = n.specimens)
            for (j in 1:n.pairs) {
                x <- pairs[1,j]
                y <- pairs[2,j]
                strain <- strain.matrix(a = a[y,x], theta = theta[y,x])
                tmp <- deformacja(data = data[,,x],strain.matrix = strain)
                tmp.a <- abind.2(tmp, data[,,y], along = 3)
                gpg <- gpagen(tmp.a,curves = curves)
                tmp.element[y,x] <- fpdist(gpg$coords[list.of.elements[[i]],,1],gpg$coords[list.of.elements[[i]],,2])
                tmp.element[x,y] <- tmp.element[y,x]
            }
            output[[i]] <- tmp.element
        }
    } else {
        if (distance == 'procrustes') {
            for (i in 1:n.elements) {
                tmp.element <- matrix(0,ncol=n.specimens,nrow = n.specimens)
                for (j in 1:n.pairs) {
                    x <- pairs[1,j]
                    y <- pairs[2,j]
                    strain <- strain.matrix(a = a[y,x], theta = theta[y,x])
                    tmp <- deformacja(data = data[,,x],strain.matrix = strain)
                    tmp.a <- abind.2(tmp, data[,,y], along = 3)
                    gpg <- gpagen(tmp.a,curves = curves)
                    tmp.element[y,x] <- proc.dist(gpg$coords[list.of.elements[[i]],,1],gpg$coords[list.of.elements[[i]],,2])
                    tmp.element[x,y] <- tmp.element[y,x]
                }
                output[[i]] <- tmp.element
            }
        } else {
            stop('wrong distance')
        }
    }
    if (!is.null(names(list.of.elements))) {
        names(output) <- names(list.of.elements)
    }
    return(output)
}