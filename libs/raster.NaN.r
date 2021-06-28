raster.NaN <- function(...) {
    r = raster(...)
    r[r>9E9] = NaN
    r
}  

brick.NaN <- function(...) {
    r = brick(...)
    r[max(r)>9E9] = NaN
    r
}
      
