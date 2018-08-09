########################################
## cfg							      ##
########################################
source("cfg.r")

limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#AA00AA", "white", "#AAAA00", "#003300")

########################################
## load and analyes  			      ##
########################################
out = makeOrLoadEnsembles()	
out = selectOutput(out)
dout = lapply(out[-1], function(i) out[[1]] - i)


########################################
## plot              			      ##
########################################
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