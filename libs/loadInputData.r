loadInputData <- function(remove = NULL, maxout = NULL, replace = NULL) {
	
    files = list.files('data/driving_Data/')
    files = files[grepl('.nc', files)]
	
    dat = lapply(paste('data/driving_Data/', files, sep = '/'), raster)
    names(dat) = unlist(strsplit(files, '.nc', fixed = TRUE))
	
    MAP_vars = grepl("MAP_", names(dat))
    dat[MAP_vars] = lapply(dat[MAP_vars], logmin)
    	
    replaceVar <- function(vname, replace) {
        if (vname == "Drought") {
            for (i in c("MADD_", "MADM_", "MDDM_", "MConc_"))
                dat = replaceVar(i, replace)
            
        } else {            
            vars = grepl(vname, names(dat))
            repl <- function(i) {
                i[!is.na(i)] = replace
                return(i)
            }
            dat[vars] = lapply(dat[vars], repl)
        }
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
