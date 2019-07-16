##################
## cfg			##
##################
source("cfg.r")
graphics.off()
limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")

llimits = c(0.2, 0.4, 0.6, 0.8)
lcols = c("FF", "BB","88", "44", "00")

items = c(2:3, 5:6)


##################
## openStuff    ##
##################

JULES_control     =  "data/JULES-mort/mort0/"
JULES_experiments =  paste0("data/JULES-mort/", c("mort1", "mortv", "mortc"))
Experiment_names  = c("100% mortality", "PFT-specific mortaility", "PFT-specific + crop masking")

JULES_control = openJulesTree(JULES_control) * 100
JULES_experiments = layer.apply(JULES_experiments, openJulesTree)
JULES_experiments = layer.apply(JULES_experiments, '*', 100)

JULES_TC = addLayer(JULES_control, JULES_experiments)
Experiment_names = c("control", Experiment_names)
        
#fireExp = c('off', 'off', 'on', 'on')
#LUExp   = c('off', 'on', 'off', 'on')
#titles = paste('JULES fire ', fireExp, ', LU ', LUExp, sep = '')
#names = paste('Fire_', fireExp, '__LU_', LUExp, sep = '')


#run with fire on, fire off; land use on; land use off

VCF_TC = loadInputData()[["TreeCover"]]
VCF_TC = raster::resample(VCF_TC, JULES_TC)
VCF_TC = VCF_TC * 100 / 0.8

out = makeOrLoadEnsembles()
lim = lapply(items, function(i) 1-selectOutput(out, i)[[1]])
lim = layer.apply(lim, mean)
lim = layer.apply(lim, raster::resample, JULES_TC)


mask = !is.na(sum(lim) + sum(JULES_TC) + VCF_TC)

lim = layer.apply(lim, function(i) max.raster(i, na.rm = TRUE) * (i-min.raster(i, na.rm = TRUE))/diff(range.raster(i, na.rm = TRUE)))
lim[[2]] = lim[[2]]^0.5
lim = layer.apply(lim, cut_results, llimits)

pcols = paste('#', lcols[lim[[1]][]], lcols[lim[[3]][]], 
			      lcols[lim[[2]][]], sep = '')
				  
cexs = (lim[[4]][]-1)/5

plotJulesVsVCFvsLim <- function(JTC, name, title) {
    fname = paste('figs/VCF_vs_JULES', name, '-maps.png', sep = '')
    png(fname, height = 4, width = 6, unit = 'in', res = 300)
	par(mfrow = c(3,1), mar = rep(0, 4))
	plotStandardMap(VCF_TC, cols = cols, limits = limits, 'VCF')
	plotStandardMap(JTC, cols = cols, limits = limits, title)
	plotStandardMap(JTC - VCF_TC, cols = dcols, limits = dlimits, 'Difference')
    dev.off()#.gitWatermark()
	
    fname = paste('figs/VCF_vs_', name, '_lims-scatter.png')
    png(fname, height = 7, width = 7, res = 300, unit = 'in')
	par(mar = c(3,3, 0.5, 0.5))
	plot(c(0, 100), c(0,100), xlab = '', ylab = '')
	lines(c(0, 100), c(0, 100), lty = 2, lwd = 2)
	mtext(side = 1, line = 2, 'VCF Cover (%)')
	mtext(side = 2, line = 2, 'Jules WOod Cover (%)')
	mtext(side = 3, line = -1, paste(title, 'error vs controls on tree cover'))

	points(VCF_TC[mask], JTC[mask], pch = 19, cex = 1.1)
	for (i in which(mask[])) {				  
		
	    points(VCF_TC[i], JTC[i], pch = 19, col = pcols[i], cex = 0.8)
	    if (cexs[i] > 0) points(VCF_TC[i], JTC[i], pch = 19, cex = cexs[i])
		
	}
    dev.off()#.gitWatermark()
}

mapply(plotJulesVsVCFvsLim, layers2list  (JULES_TC), c('mort0', 'mort1', 'mortc', 'mortv'), Experiment_names)
