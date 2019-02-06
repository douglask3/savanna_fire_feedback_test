########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")

########################################
## plot              			      ##
########################################

plotExps <- function(fname, ExpID, out, dout) {
    fname = paste0('figs/TreeCover_ensemble_summary-', fname, c('-diff', '-abs'), '.png')

    ExpIDp = 1:length(ExpID)
    if (!is.even(length(ExpID))) ExpIDp = c(ExpIDp, NaN)
    ExpIDp = ExpIDp - min(ExpIDp, na.rm = TRUE) + 1
	
	
    p_rows = (length(ExpIDp)/2)
	
    png(fname[1], height = 5 * (p_rows + 1.6)/4.6, width = 10, unit = 'in', res = 300)
		
        lmat = rbind(1:2, 3,t(matrix(ExpIDp, nrow = 2)) + 3, max(ExpIDp, na.rm = TRUE) + 4)
	lmat[is.na(lmat)] = 0.0
	
	layout(lmat, heights = c(1, 0.3, rep(1, p_rows), 0.3))
	par(mar = rep(0, 4))
	
	plotStandardMap(dats[[1]][['TreeCover']] * 100, limits = limits, cols = cols, '')
	plotStandardMap.sd(out[[1]], 100, limits = limits, cols = cols, 'reconstructed')
	plot.new()#addStandardLegend(out[[1]], limits, cols, add = FALSE)
	mtext('', side = 1, line = 0, exp = NA)
	mapply(plotStandardMap.sd, dout[(ExpID - 1)], txt = '', MoreArgs = list(100, limits = dlimits, cols = dcols))
	plot.new()#addStandardLegend(dout[[1]], dlimits, dcols, add = FALSE)
    dev.off.gitWatermark()

    png(fname[2], height = 5 * (p_rows + 1.3)/4.6, width = 10, unit = 'in', res = 300)
	
	lmat = rbind(1:2, t(matrix(ExpIDp, nrow = 2)) + 2, max(ExpIDp, na.rm = TRUE) + 3)
	lmat[is.na(lmat)] = 0.0
		
	layout(lmat, heights = c(1, rep(1, p_rows), 0.3))
	par(mar = rep(0, 4))
	plotStandardMap(dats[[1]][['TreeCover']] * 100, limits = limits, cols = cols, '')
	
	names = c('reconstructed', paste('No', expNames[ExpID]))	
	mapply(plotStandardMap.sd, out[c(1,ExpID)], txt = '', MoreArgs = list(100, limits = limits, cols = cols))
	plot.new()#addStandardLegend(out[[2]], limits, cols, add = FALSE)
    dev.off.gitWatermark()
}

PlotAllExperiments <- function(...) {
    ########################################
    ## load and analyes  		  ##
    ######################################## 
    out = makeOrLoadEnsembles(...)	
    out = selectOutput(out)
    dout = lapply(out[-1], function(i) out[[1]] - i)

       
    plotExps_fun <- function(name, ExpID) {
        fname = paste(name, ..., sep = '-') 
        plotExps(fname, ExpID, out = out, dout = dout)
    }

    #plotExps_fun('mortalityAndExclusion', 5:12)
    #plotExps_fun('MAPvsNonClim', c(2,4))
    #plotExps_fun('allVars', 2:12)
    plotExps_fun('Controls', c(2, 3, 12, 13))
}

runAll_pr_droughts(PlotAllExperiments)	
