raster.NaN <- function(...) {
    r = raster(...)
    r[r>9E9] = NaN
    r
}  

brick.NaN <- function(..., layers = NULL) {
    r = brick(...)
    if (!is.null(layers)) r = r[[layers]]
    r[max(r)>9E9] = NaN
    r
}
      
