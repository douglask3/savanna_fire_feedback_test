logmin <- function(x, xmin = 0.00001) {
    x[x < xmin] = xmin
    return(log(x))
}