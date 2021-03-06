########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

limits = c(1, 10, 20, 30, 40, 50, 60, 70, 80)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(1, 2, 5, 10, 20, 40)
dcols = c("white", "#FF33FF", "#DD0033", "#330000")
dcols = c("#ffffe5", "#d9f0a3", "#78c679", "#238443", "#004529", "#002211")
dcols = rev(c('#a00532','#c51b7d','#f1b6da','#f7f7f7'))

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#1A001A", "#330033", "#662366", "#AA78AA", "#eed9ee", "#ffffe5", "#d9f0a3", "#78c679", "#238443", "#004529", "#002211")

########################################
## plot              			      ##
########################################

plotExps <- function(fname1, fname2, ExpID, out, dout, plotFullModOnly = FALSE, obsOnly = FALSE) {
    fname = paste0('figs/TreeCover_ensemble_summary-',
                    fname1, fname2, c('-diff', '-abs'), '.png')
    

    ExpIDp = 1:length(ExpID)
    if (!is.even(length(ExpID))) ExpIDp = c(ExpIDp, NaN)
    ExpIDp = ExpIDp - min(ExpIDp, na.rm = TRUE) + 1
    
    p_rows = (length(ExpIDp)/2)
	
    if (!plotFullModOnly && !obsOnly) {
        png(fname[1], height =(4.75/6.5) * 5 * (p_rows + 1.6)/4.6, width = 4.75, unit = 'in', res = 300)	
        lmat = rbind(1:2, 3,t(matrix(ExpIDp, nrow = 2)) + 3, max(ExpIDp, na.rm = TRUE) + 4)
        lmat[is.na(lmat)] = 0.0
	
        layout(lmat, heights = c(1, 0.33, rep(1, p_rows), 0.33))
        par(mar = c(0, 0, 1.5, 0), oma = c(0, 0, 1.5, 0))
    }
    if (obsOnly) obsTitle  = '' else obsTitle = 'VCF'
    if (plotFullModOnly) modTitle = '' else modTitle = 'reconstructed'
	
    if (!plotFullModOnly)
        plotStandardMap(dats[[1]][['TreeCover']] * 100/0.8, limits = limits, cols = cols, obsTitle, mtext_line = 0.5)
    if (obsOnly) return()
    
    plotStandardMap.sd(out[[1]], 100, limits = limits, cols = cols, modTitle, mtext_line = 0.5)
    if (plotFullModOnly) return()
    par(mar = c(0.5, 0, 0, 0))
	addStandardLegend(out[[1]], limits, cols, add = FALSE, maxLab = '100',
                          plot_loc = c(0.15, 0.85, 0.7, 0.9))
    par(mar = c(0, 0, 1.5, 0))

	#mtext('Impact of ...', side = 1, line = 0, exp = NA)
        txt = paste0(LETTERS[1:length(ExpID)], '  ', expNames[ExpID])
	mapply(plotStandardMap.sd, dout[(ExpID - 1)], txt = txt,
               MoreArgs = list(100, limits = dlimits, cols = dcols, 
               limit_error = c(0.05, 0.1, 0.25), ePatternThick = 0.25,
               mtext_line = 0.5))

        par(mar = rep(0,4))
	    addStandardLegend(dout[[2]], dlimits, dcols, add = FALSE, extend_max = TRUE,
                              extend_min = TRUE,
                              plot_loc = c(0.1, 0.9, 0.7, 0.9))
        par(mar = c(0, 0, 1.5, 0))
    dev.off()#.gitWatermark()

    png(fname[2], height = 5 * (p_rows + 1.3)/4.6, width = 8, unit = 'in', res = 300)
	
	lmat = rbind(1:2, t(matrix(ExpIDp, nrow = 2)) + 2, max(ExpIDp, na.rm = TRUE) + 3)
	lmat[is.na(lmat)] = 0.0
		
	layout(lmat, heights = c(1, rep(1, p_rows), 0.33))
	par(mar = c(0, 0, 1.5, 0), oma = c(0, 0, 1.5, 0))
	plotStandardMap(dats[[1]][['TreeCover']] * 100/0.8, limits = limits, cols = cols, 'VCF')
	
	names = c('reconstructed', paste('No', expNames[ExpID]))
        	
	mapply(plotStandardMap.sd, out[c(1,ExpID)], txt = names, MoreArgs = list(100, limits = limits, cols = cols))

    par(mar = rep(0,4))
	addStandardLegend(out[[2]], limits, cols, add = FALSE, maxLab = '100')
    dev.off()#.gitWatermark()
}

PlotAllExperiments <- function(fire_dataset, pr_dataset, drought_var, ..., fnameExtra = '', 
                               plotFullModOnly = FALSE, obsOnly = FALSE, normalise = FALSE) {
    ########################################
    ## load and analyes  		  ##
    ######################################## 
    
    out = makeOrLoadEnsembles(fire_dataset = fire_dataset, pr_dataset = pr_dataset, drought_var = drought_var, ...)
    out = selectOutput(out)
    out = lapply(out, function(i) i/0.8)
    
    if (normalise) {
        nFun <- function(en, i) {
            #if (en == 1) browser()
            #(i[[en]]- - out[[5]][[en 
            r = abs(i[[en]] - out[[1]][[en]])/max(addLayer(i[[en]], out[[1]][[en]]))
            r[out[[5]][[en]] <0.01] = NaN
            r
        }
        #browser()
        dout = lapply(out[-1], function(i)
                layer.apply(1:nlayers(i), nFun, i))
        out = lapply(out, function(i) i/out[[1]])
        fnameN = "normalise"
    } else {
        dout = lapply(out[-1], function(i) i - out[[1]])
        fnameN = ""
    }

    plotExps_fun <- function(name, ExpID) {
        fname = paste(name, fnameExtra,..., sep = '-') 
        plotExps(fname, fnameN, ExpID, out = out, dout = dout,
                 plotFullModOnly = plotFullModOnly,
                 obsOnly = obsOnly)
    }
    #browser()
    plotExps_fun('JULEScomparison', c(2, 5, 9:15))
    plotExps_fun('JULEScomparison2', c(2, 5, 10:11, 16))
    plotExps_fun('mortalityAndExclusion', c(5, 9:16))
    plotExps_fun('fireExps', c(5,6))

    #plotExps_fun('RainfallDist', c(7:9))

    
    if (obsOnly) return()
    if (plotFullModOnly) {
        let = LETTERS[ which(pr_dataset == pr_datasets) + 
                      (which(drought_var == drought_vars) - 1) *4 + 1]
        txt = paste(let, pr_dataset, drought_var)
        mtext(txt, line = 1)
        return(out[[1]])
    }
    #plotExps_fun('MAPvsNonClim', c(2,4))
    #plotExps_fun('allVars', 2:14)
    #plotExps_fun('Controls', c(2, 3, 15, 16))
}

#PlotAllExperiments(pr_dataset = 'GPCC', drought_var = 'MADM')
#PlotAllExperiments(pr_dataset = 'GPCC', drought_var = 'MConc')
#PlotAllExperiments(pr_dataset = 'GPCC', drought_var = 'MDDM')

#PlotAllExperiments(pr_dataset = 'CMORPH', drought_var = 'MADM')
#PlotAllExperiments(pr_dataset = 'CMORPH', drought_var = 'MConc')
#PlotAllExperiments(pr_dataset = 'CMORPH', drought_var = 'MDDM')

#PlotAllExperiments(pr_dataset = 'MSWEP', drought_var = 'MADD', normalise = TRUE)
#PlotAllExperiments(fire_dataset = 'GFED_four_s', pr_dataset = 'MSWEP', andFire = FALSE, drought_var = 'MADD')
PlotAllExperiments(fire_dataset = 'GFED_four_s', pr_dataset = 'MSWEP', andFire = FALSE, drought_var = 'MADD', normalise = TRUE)
PlotAllExperiments(fire_dataset = 'GFED_four_s', pr_dataset = 'MSWEP', andFire = TRUE, fnameExtra = 'withourFire', drought_var = 'MADD')
PlotAllExperiments(fire_dataset = 'GFED_four_s', pr_dataset = 'MSWEP', andFire = c(FALSE, TRUE), fnameExtra = 'FireInteraction', drought_var = 'MADD')
#, andFire = c(F, T)
lmat = c(1, 0, 0, 0)
lmat = rbind(lmat, matrix(2:17, ncol = 4))
lmat = rbind(lmat, max(lmat) + 1, max(lmat) + 2)

png('figs/VCF_vs_mod.png', height = 2.2*2, width = 4.75*2, units = 'in', res = 300)
    layout(lmat, heights = c(1, 1, 1, 1, 1, 0.4, 0.4))
    par(mar = c(0, 0, 1.5, 0), oma = c(0, 0, 1.5, 0))
    PlotAllExperiments(fire_dataset = 'GFED_four_s',pr_dataset = 'MSWEP', drought_var = 'MADD', obsOnly = TRUE)
    mtext('A VCF', line = 1)

    out = runAll_pr_droughts(PlotAllExperiments, plotFullModOnly = TRUE)
    par(mar = rep(0,4))	
    addStandardLegend(out[[1]][[1]], limits, cols, add = FALSE, maxLab = '100')
dev.off()


