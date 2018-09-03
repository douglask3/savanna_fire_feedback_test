openJulesTree <- function(dir, levels = c(1,2,5), varname = 'landCoverFrac') {
	files = list.files(dir, full.names=TRUE)
	yrs = sapply(files, function(i) strsplit(i, 'Monthly.')[[1]][2])
	yrs = as.numeric(unlist(strsplit(yrs, '.nc')))
	index = yrs >= 2000 & yrs <= 2014
	files = files[index]
	TC = layer.apply(files, process.jules.file, levels, varname)
	TC = mean(TC)

	TC = convert_pacific_centric_2_regular(TC)
}