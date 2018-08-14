loadInputData <- function(remove = NULL, maxout = NULL, replace = NULL) {
	files = list.files('data/')
	files = files[grepl('.nc', files)]
	
	dat = lapply(paste('data', files, sep = '/'), raster)
	names(dat) = unlist(strsplit(files, '.nc'))
	
	dat[['MAP']] = logmin(dat[['MAP']])
	
	replaceVar <- function(vname, replace) {
		dat[[vname]][!is.na(dat[[vname]])] = replace
		return(dat)
	}	
	
	if (!is.null(remove))
		for (i in remove) dat = replaceVar(i, 0.0)
	
	if (!is.null(maxout))
		for (i in maxout) dat = replaceVar(i, 9E9)
	
	if (!is.null(replace))
		for (i in 1:length(replace)) dat = replaceVar(names(replace)[i], replace[i])
		
	
	return(dat)
}