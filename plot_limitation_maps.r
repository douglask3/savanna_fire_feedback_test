########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")


limits = c(0.2, 0.4, 0.6, 0.8)
cols = c("FF", "BB","88", "44", "00")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")
items = c(2:3, 5:6)

########################################
## load and analyes  			      ##
########################################

plot4pr_droughts <- function(pr_dataset, drought_var) {
    out = makeOrLoadEnsembles(pr_dataset = pr_dataset, drought_var = drought_var)
    lim = lapply(items, function(i) 1-selectOutput(out, i)[[1]])
    sen = lapply(items, function(i)  tail(selectOutput(out, i), 1)[[1]])

    xy = xyFromCell(lim[[1]], 1:length(lim[[1]]))
    mlim = lim

    nlim = mlim[[1]] + mlim[[2]] + mlim[[3]] + mlim[[4]]
    nlim = lapply(mlim, '/', nlim)

    plotMap <- function(x, fname = '', normalise = FALSE) {
	x = lapply(x, function(i) max.raster(i, na.rm = TRUE) * (i-min.raster(i, na.rm = TRUE))/diff(range.raster(i, na.rm = TRUE)))
	
	#2.5:4.5
	pout = lapply(x, values)
	
	if (normalise) cols = rev(cols)
	# if rev: c,m,y else: c,m,y
	pout[[2]] = pout[[2]] ^0.33
	plot_4way(xy[,1], xy[,2], pout[[1]], pout[[3]], pout[[2]], pout[[4]],
			  x_range = c(-120, 160), y_range = c(-30, 30),
			  cols = c(cols), limits = c(0.2, 0.4, 0.6, 0.8), 
			  coast.lwd=par("lwd"),ePatternRes = 35, ePatternThick = 0.4,
			  add_legend=FALSE, smooth_image=FALSE,smooth_factor=5, normalise = normalise)
	
    }	

    plotMaps <- function(x, fname, ...) {
	plotStuff <- function(ID1, FUN1, FUN2, FUN3 = NULL) {
            x0 = x
	    x[[ID1]] = FUN1(x[[ID1]])
	    x[-ID1] = lapply(x[-ID1], FUN2)
            if (!is.null(FUN3)) x = mapply(function(x, x0) x - FUN3(x0), x, x0)
	    plotMap(x, ...)
	}

        fname = paste('figs/limPlot', fname, '.png', sep = '')
	png(fname, height = 6, width = 12, res = 300, unit = 'in')
	    print(fname)
            
	   # layout(rbind(t(matrix(1:6, ncol = 3)), 7), heights = c(1,1,1,0.5))
	    par(mfrow = c(3, 2), mar = c(1, 0, 1, 0), oma = c(0,0,0.5, 0))
	    plotStuff(1, mean, mean)
            
            add_raster_4way_legend(cols = cols ,limits = c(0.2, 0.4, 0.6, 0.8),
				   labs = c('<- Moisture', 'Fuel ->', 'Igntions ->', 'Land Use'))

	    mapply(plotStuff, 1:2, MoreArgs = list( function(i) max(i),  function(i) min(i)))
	    #mapply(plotStuff, 1:4, paste('min', 1:4), MoreArgs = list( function(i) min(i),  function(i) max(i)))

	    
        
        dev.off.gitWatermark()
    }
    
    fname = paste0(pr_dataset, '-', drought_var, '-')
    plotMaps(mlim, paste0(fname, 'standard'))
    #plotMaps(nlim, paste0(fname, 'potential'))
    plotMaps(sen, paste0(fname, 'sensitivity'))
	
}

plot4pr_droughts('MSWEP', 'MADD')
#runAll_pr_droughts(plot4pr_droughts)

