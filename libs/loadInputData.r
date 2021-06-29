loadInputData <- function(remove = NULL, maxout = NULL, replace = NULL, fireNudge = NULL) {
	
    files = list.files('data/driving_Data/')
    files = files[grepl('.nc', files)]
	
    dat = lapply(paste('data/driving_Data/', files, sep = '/'), raster)
    
    names(dat) = unlist(strsplit(files, '.nc', fixed = TRUE))
	
    MAP_vars = grepl("MAP_", names(dat))
    #dat[MAP_vars] = lapply(dat[MAP_vars], logmin)

   
    droughtMetrics = c("MADD_", "MADM_", "MDDM_", "MConc_")
    splitDrought <- function(metric) {
        nms = names(dat)
        index = which(grepl(metric,nms))
        new_dat = rep(dat[index], each = 2)
        new_nms = paste0(c('RainTerm_', 'StressTerm_'), rep(nms[index],each = 2))
        names(new_dat) = new_nms
        return(new_dat)
    }
    new_dat = lapply(droughtMetrics, splitDrought)
    dat = c(dat, unlist(new_dat))
	
    replaceVar <- function(dat, vname, replace) {
       
        if (vname == "Drought") {
            for (i in droughtMetrics)
                dat = replaceVar(dat, i, replace)
            
        } else if (vname == "RainTerm_Drought") {
            for (i in droughtMetrics)
                dat = replaceVar(dat, paste0("RainTerm_", i), replace)
        } else if (vname == "StressTerm_Drought") {
            for (i in droughtMetrics)
                dat = replaceVar(dat, paste0("StressTerm_", i), replace)
        } else {   
            #if (vname == droughtMetrics[1]) browser()  
            vars = grepl(vname, names(dat))
            repl <- function(i) {
                if (is.character(replace)) {
                    rn = raster(replace)
                    rn = raster::resample(rn, i)
                    i = rn
                } else i[!is.na(i)] = replace
                return(i)
            }
            dat[vars] = lapply(dat[vars], repl)
        }
	return(dat)
    }	
    #if (remove == "RainTerm_Drought") browser()
   
    if (!is.null(remove)) {
	
        "if (!is.null(fireNudge)) {
            nudge <- function(x, var) {
                kc = conFire_params[, x[1]] * conFire_params[, x[2]]
                kc = quantile(kc, c(0.05, 0.5, 0.95))
                S = layer.apply(kc, function(i) exp(i * dat[[var]]))
                return(S)
            }
            Ss = mapply(nudge, fireNudge, names(fireNudge))
            browser()
        }"
        for (i in remove) dat = replaceVar(dat, i, 0.0)
    }
    	
    if (!is.null(maxout))
	for (i in maxout) dat = replaceVar(dat, i, 9E9)
	
    if (!is.null(replace))
	for (i in 1:length(replace)) dat = replaceVar(dat, names(replace)[i], replace[i])
		
    return(dat)
}
