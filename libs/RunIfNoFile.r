runIfNoFile <- function(file, FUN, ..., test = TRUE) {
    if (test && all(file.exists(file))) {
        if (length(file) > 1) return(lapply(file, stack))
        if (tail(strsplit(file, '.', TRUE)[[1]],1) == "csv") return(read.csv(file)[,-1])
        return(stack(file))
    }
    
    r = FUN(...)
    
    if (is.raster(r)) r = writeRaster(r, file, overwrite = TRUE)
    else if (class(r) == 'list')
        r = mapply(writeRaster, r, file, MoreArgs = list(overwrite = TRUE))
    else if (is.data.frame(r) || is.matrix(r) || is.numeric(r)) 
        write.csv(r, file)
    else stop('unknown variable class')
    
    return(r)
}
    
