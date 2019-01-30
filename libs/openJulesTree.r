openJulesTree <- function(dir, levels = c(1:5, 12:13), varname = 'landCoverFrac', splitPFTs = FALSE) {

	files = list.files(dir, full.names=TRUE)
	yrs = sapply(files, function(i) strsplit(i, 'Monthly.')[[1]][2])
	if (all(is.na(yrs))) yrs = sapply(files, function(i) strsplit(i, 'Annual.')[[1]][2])
	yrs = as.numeric(unlist(strsplit(yrs, '.nc')))
	index = yrs >= 2000 & yrs <= 2014
	
	files = files[index]
    openFun <- function(level) {
        TC = layer.apply(files, process.jules.file, level, varname)
        TC = mean(TC)
        return(TC)
    }
    if (splitPFTs)  TC = layer.apply(levels, openFun)
        else TC = openFun(levels)
	TC = convert_pacific_centric_2_regular(TC)
    return(TC)
}