source("cfg.r")


dats = list(control   = loadInputData(),
				nofire    = loadInputData(remove = c("BurntArea")),
				noDrought = loadInputData(remove = c("Drought")),
				noPop     = loadInputData(remove = c("PopDen")),
				noCrop    = loadInputData(remove = c("crop")),
				noPas     = loadInputData(remove = c("pas")),
				noHumans  = loadInputData(remove = c("PopDen", "urban", "crop", "pas")))
				
expNames = c('Control', 'fire', 'drought', 'population effect', 'cropland', 'pasture', 'humans')

makeOrLoadEnsembles <- function(grab_cache = TRUE, invert =TRUE) {
	

	run_member <- function(line) {
		dname = paste(ens_dir, 'ensemble_', line, '/', sep = '')
		makeDir(dname)
		
		fnames =  paste(dname, names(dat), '.nc', sep = '')
		
		run <- function(dat, fname)
			out = runIfNoFile(fname, runLimTREE, line, dat, test = grab_cache)
		
		out = mapply(run, dats, fnames)
	}

	invertEnsemble <- function(ensembles)
		lapply(1:length(ensembles[[1]]), function(j) lapply(ensembles, function(i) i[[j]]))

	out = lapply(ensemble_no, run_member)
	if (invert) out = invertEnsemble(out)
	return(out)
}

out = makeOrLoadEnsembles()

selectOutput <- function(ensembles, item = 1)
	lapply(ensembles, function(j) layer.apply(j, function(i) i[[item]]))
	
out = selectOutput()
dout = lapply(out[-1], function(i) out[[1]] - i)

plotStandardMap <- function(x, limits, cols, txt = '', add_legend = FALSE,...) {
	
    plot_raster_from_raster(x, limits = limits, cols = cols,
                            transpose = FALSE, srt = 0,
                            plot_loc = c(0.35,0.83,0.01,0.04),
                            quick = TRUE, add_legend = add_legend, ...)
    mtext(txt,side = 3, line = -1)
}

plotStandardMap.sd <- function(x, sc = 1, ...)
	plotStandardMap(mean(x) * sc, 
					e = sd.raster(x), ePatternRes = 30, ePatternThick = 0.2, limits_error = c(1/10, 1/2),
					...)
		
addStandardLegend <- function(x, limits, cols, units = '%',...)  {
	add_raster_legend2(cols, limits, dat = x, srt = 0,
			   transpose = FALSE, plot_loc = c(0.25, 0.75, 0.5, 0.8), ylabposScling=1.5, oneSideLabels = NA, ...)
	mtext(units, side = 3, lines = -1)
}

limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#AA00AA", "white", "#AAAA00", "#003300")

png('figs/TreeCover_ensemble_summary.png', height = 5, width = 10, unit = 'in', res = 300)
	layout(rbind(1:2, 3, 4:5, 6:7, 8:9, 10), heights = c(1, 0.3, 1, 1, 1, 0.3))
	par(mar = rep(0, 4))
	plotStandardMap(dats[[1]][['TreeCover']] * 100, limits = limits, cols = cols, 'VCF')
	plotStandardMap.sd(out[[1]], 100, limits = limits, cols = cols, 'reconstructed')
	addStandardLegend(out[[1]], limits, cols, add = FALSE)

	mtext('Impact of ...', side = 1, line = 0, exp = NA)
	mapply(plotStandardMap.sd, dout, txt = expNames[-1], MoreArgs = list(100, limits = dlimits, cols = dcols))
	addStandardLegend(dout[[1]], dlimits, dcols, add = FALSE)
dev.off.gitWatermark()

png('figs/TreeCover_ensemble_summary_abs.png', height = 5 * 4.3/4.6, width = 10, unit = 'in', res = 300)
	layout(rbind(1:2, 3:4, 5:6, 7:8, 9), heights = c(1, 1, 1, 1, 0.3))
	par(mar = rep(0, 4))
	plotStandardMap(dats[[1]][['TreeCover']] * 100, limits = limits, cols = cols, 'VCF')
	
	names = c('reconstructed', paste('No', expNames[-1]))	
	mapply(plotStandardMap.sd, out, txt = names, MoreArgs = list(100, limits = limits, cols = cols))
	addStandardLegend(dout[[1]], dlimits, dcols, add = FALSE)
dev.off.gitWatermark()