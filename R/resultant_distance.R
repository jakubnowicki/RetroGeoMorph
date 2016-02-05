#' Calculate resultant distance
#' 
#' Calculate resultant distance from several partial distances as sum, mean or resultant vector
#'
#' @param data vector of distances
#' @param method method of calculation (sum, mean or vector)
#' @export


resultant.distance <- function(data, method) {
    if (method=='mean') {
        output <- mean(data)
    } else {
        if (method=='sum') {
            output <- sum(data)
        } else {
            if (method == 'vector') {
                output <- sqrt(sum(data^2))
            } else {
                stop('wrong method')
            }
        }
    }
    return(output)
}