polygonDist <- function(x, y1, y2 = NULL, ...) {
    if (is.null(y2)) {
        xs = c(x[1], x, tail(x, 1)) 
        ys = c(0, y1, 0)
    } else {
        xs = c(x, rev(x))
        ys = c(y1, rev(y2))
    }
    
    polygon(xs, ys, ...)
} 
