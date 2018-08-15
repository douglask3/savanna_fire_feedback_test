source("cfg.r")
graphics.off()

dat = loadInputData()
params = read.csv(paramFile, stringsAsFactors=FALSE)

out = makeOrLoadEnsembles()	
out = selectOutput(out)

dout = lapply(out[-1], function(i) 1-out[[1]]/i)
png('figs/BA_impact.png', width = 5, height = 5, res = 300, unit = 'in')
	par(mar = c(3, 3, 1, 1))
	plot(c(0.001, 1), c(0, 1), axes = FALSE, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', log = 'x', type ='n')
	
	labs = c(0.001, 0.003, 0.01, 0.03, 0.3, 0.3, 1)
	axis(1, at = labs, labels = labs * 100) 
	mtext(side = 1, 'Burnt Area (%)', line = 2)

	labs = seq(0, 1, 0.2)
	axis(2, at = labs, labels = labs * 100) 
	mtext(side = 2, 'Impact on Tree Cover (%)', line = 2)

	addPolygon <- function(y, x = dat[['BurntArea']]) { 
		#browser()
		quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.0, 1.0), between = TRUE, col = make.transparent('black', 0.99))
		quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.1, 0.9), between = TRUE, col = make.transparent('#330000', 0.99))
		quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.25, 0.75), between = TRUE, col = make.transparent('red', 0.98))
	}
	layer.apply(dout[[4]], addPolygon)
	x = seq(0, 1, 0.001)
	#lines(x, 1-logistic(x, params[1, 'mort_x0'], -params[1, 'mort_k'])/logistic(0, params[1, 'mort_x0'], -params[1, 'mort_k']), col = 'red')
	lines(x,x, lty = 2,lwd = 2)


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

	Jules_fire_on_fname = '../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF3-2/'
	Jules_fire_off_fname = '../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF2/'

	Jules_fire = openJulesTree(Jules_fire_on_fname,  1, "burnt_area_gb") * 60 * 60 * 24 * 365
	Jules_TC_fire_on = openJulesTree(Jules_fire_on_fname)
	Jules_TC_fire_off = openJulesTree(Jules_fire_off_fname)

	Jules_dout = (Jules_TC_fire_off - Jules_TC_fire_on)/max(addLayer(Jules_TC_fire_off, Jules_TC_fire_on))
	#addPolygon(Jules_dout, Jules_fire)

	#quantileDesnityPoly(Jules_fire[], Jules_dout[], xlim = c(0, 1), nbins = 10, quantiles = c(0.1, 0.25, 0.75, 0.9), between = TRUE, col = make.transparent('blue', 0.9))
	points(Jules_fire[], Jules_dout[], pch = 19 , col = make.transparent('blue', 0.9), cex = 0.3)

	mask = !is.na(Jules_fire + Jules_dout) & Jules_dout > 0 & Jules_fire > 0.001

	x = log10(Jules_fire[mask])
	y = Jules_dout[mask]

	fit = nls(y ~ logistic(x, x0, k), start = c(x0 = -1, k = 6), lower = c(-10, 4), algorithm = "port")
	#fit = lm(y~x)

	xp = seq(-5, 1, length.out = 1001)
	yp = predict(fit, data.frame(x = xp))
	lines((10^xp), yp, col = 'blue', lwd = 2)

	legendFun <- function(col, lwd)
		legend('topleft', legend = c('JULES', '   - best fit', 'Bayes', '1:1'), 
			   bty = 'n', pch = 19, pt.cex = c(0.5, 0.0, 0.0, 0.0), col = c('blue', 'blue', col, 'black'), 
			   lty = c(0, 1, 1, 2), lwd = c(0, 2, lwd, 2))
		

	for (i in 1:(floor(length(ensemble_no))/3)) {
		legendFun(make.transparent('black', 0.99), 18)
		legendFun(make.transparent('black', 0.99), 15)
		legendFun(make.transparent('#330000', 0.99), 12)
		legendFun(make.transparent('#330000', 0.99), 9)
		legendFun(make.transparent('red', 0.98), 6)
		legendFun(make.transparent('red', 0.98), 3)
	}
dev.off.gitWatermark()