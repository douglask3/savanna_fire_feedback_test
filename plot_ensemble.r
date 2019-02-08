########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

limits = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(1, 2, 5, 10, 20, 40)
dcols = c("white", "#FF33FF", "#DD0033", "#330000")
dcols = c("#ffffe5", "#d9f0a3", "#78c679", "#238443", "#004529", "#002211")

########################################
## plot              			      ##
########################################

plotExps <- function(fname, ExpID, out, dout) {
    fname = paste0('figs/TreeCover_ensemble_summary-', fname, c('-diff', '-abs'), '.png')

    ExpIDp = 1:length(ExpID)
    if (!is.even(length(ExpID))) ExpIDp = c(ExpIDp, NaN)
    ExpIDp = ExpIDp - min(ExpIDp, na.rm = TRUE) + 1
    
    p_rows = (length(ExpIDp)/2)
	
    png(fname[1], height = 5 * (p_rows + 1.6)/4.6, width = 8, unit = 'in', res = 300)	
        lmat = rbind(1:2, 3,t(matrix(ExpIDp, nrow = 2)) + 3, max(ExpIDp, na.rm = TRUE) + 4)
	lmat[is.na(lmat)] = 0.0
	
	layout(lmat, heights = c(1, 0.33, rep(1, p_rows), 0.3))
	par(mar = c(0, 0, 1.5, 0), oma = c(0, 0, 1.5, 0))
	
	plotStandardMap(dats[[1]][['TreeCover']] * 100/0.8, limits = limits, cols = cols, 'VCF')
	plotStandardMap.sd(out[[1]], 100, limits = limits, cols = cols, 'reconstructed')

        par(mar = c(0.5, 0, 0, 0))
	    addStandardLegend(out[[1]], limits, cols, add = FALSE, maxLab = '100', )
        par(mar = c(0, 0, 1.5, 0))

	#mtext('Impact of ...', side = 1, line = 0, exp = NA)
	mapply(plotStandardMap.sd, dout[(ExpID - 1)], txt = expNames[ExpID], 
               MoreArgs = list(100, limits = dlimits, cols = dcols))

        par(mar = rep(0,4))
	    addStandardLegend(dout[[1]], dlimits, dcols, add = FALSE)
        par(mar = c(0, 0, 1.5, 0))
    dev.off.gitWatermark()

    png(fname[2], height = 5 * (p_rows + 1.3)/4.6, width = 8, unit = 'in', res = 300)
	
	lmat = rbind(1:2, t(matrix(ExpIDp, nrow = 2)) + 2, max(ExpIDp, na.rm = TRUE) + 3)
	lmat[is.na(lmat)] = 0.0
		
	layout(lmat, heights = c(1, rep(1, p_rows), 0.3))
	par(mar = c(0, 0, 1.5, 0), oma = c(0, 0, 1.5, 0))
	plotStandardMap(dats[[1]][['TreeCover']] * 100/0.8, limits = limits, cols = cols, 'VCF')
	
	names = c('reconstructed', paste('No', expNames[ExpID]))
        	
	mapply(plotStandardMap.sd, out[c(1,ExpID)], txt = names, MoreArgs = list(100, limits = limits, cols = cols))

        par(mar = rep(0,4))
	addStandardLegend(out[[2]], limits, cols, add = FALSE, maxLab = '100')
    dev.off.gitWatermark()
}

PlotAllExperiments <- function(...) {
    ########################################
    ## load and analyes  		  ##
    ######################################## 
    out = makeOrLoadEnsembles(...)	
    out = selectOutput(out)
    out = lapply(out, function(i) i/0.8)
    dout = lapply(out[-1], function(i) i - out[[1]])
       
    plotExps_fun <- function(name, ExpID) {
        fname = paste(name, ..., sep = '-') 
        plotExps(fname, ExpID, out = out, dout = dout)
    }

    plotExps_fun('mortalityAndExclusion', 5:12)
    plotExps_fun('MAPvsNonClim', c(2,4))
    plotExps_fun('allVars', 2:12)
    plotExps_fun('Controls', c(2, 3, 12, 13))
}

#PlotAllExperiments(pr_dataset = 'GPCC', drought_var = 'MADM')
#PlotAllExperiments(pr_dataset = 'GPCC', drought_var = 'MConc')
#PlotAllExperiments(pr_dataset = 'GPCC', drought_var = 'MDDM')

#PlotAllExperiments(pr_dataset = 'CMORPH', drought_var = 'MADM')
#PlotAllExperiments(pr_dataset = 'CMORPH', drought_var = 'MConc')
#PlotAllExperiments(pr_dataset = 'CMORPH', drought_var = 'MDDM')

PlotAllExperiments(pr_dataset = 'MSWEP', drought_var = 'MADD')
#runAll_pr_droughts(PlotAllExperiments)	



