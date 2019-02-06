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
	fname = paste('figs/limPlot', fname, '.png', sep = '')
	png(fname, height = 6, width = 12, res = 300, unit = 'in')
	print(fname)
	lmat = rbind(c(1,2),
				 c(3,4),
				 5,
				 6)
	
	layout(lmat, heights = c(1,1,2,0.5))
	par(mar = rep(0,4), oma = c(0,0,0.5, 0))	
	x = lapply(x, function(i) max.raster(i, na.rm = TRUE) * (i-min.raster(i, na.rm = TRUE))/diff(range.raster(i, na.rm = TRUE)))
	
	x[c(1, 3)] = lapply(x[c(1, 3)], function(i) {i[1:20, 345:420][is.na(i[1:20, 345:420])] = max.raster(i, na.rm = TRUE); i})
	x[c(2, 4)] = lapply(x[c(2, 4)], function(i) {i[1:20, 345:420][is.na(i[1:20, 345:420])] = quantile(i, 0.1); i})
	
	plotFun <- function(i, cols, title) {
	    plot_raster_from_raster(i, cols = cols, limits = seq(0.0, 1.0, 0.1), add_legend=FALSE, interior = FALSE)
            addStandardLegend(i, rev(seq(0.0, 1.0, 0.1) * 100) , cols, units = '')
	    mtext(side = 3, adj = 0.2, title, line = -1.15, cex = 1.5)
	}
	
	
	mapply(plotFun, x, 
	       cols = list(c("white", '#00FFFF', '#000011'),
			   c("white", '#888800', "#001100"),
			   c("white", "#FF00FF", "#110000"),
			  c("white", "black")), plot_title)
	
	
	#2.5:4.5
	pout = lapply(x, values)
	
	if (normalise) cols = rev(cols)
	# if rev: c,m,y 
	#		  c,m,y
	pout[[2]] = pout[[2]] ^0.33
	plot_4way(xy[,1], xy[,2], pout[[1]], pout[[3]], pout[[2]], pout[[4]],
			  x_range = c(-180, 180), y_range = c(-30, 30),
			  cols = 	c(cols), limits = c(0.2, 0.4, 0.6, 0.8), 
			  coast.lwd=par("lwd"),ePatternRes = 35, ePatternThick = 1.0,
			  add_legend=FALSE, smooth_image=FALSE,smooth_factor=5, normalise = normalise)

	
	#par(mar = c(3, 10, 0, 8))
	add_raster_4way_legend(cols = cols ,limits = c(0.2, 0.4, 0.6, 0.8),
					   labs = c('<- Moisture', 'Fuel ->', 'Igntions ->', 'Land Use'))
	dev.off.gitWatermark()
}	
plotMaps <- function(x, fname, ...) {
	plotStuff <- function(ID1, ext, FUN1, FUN2) {
		fname = paste(fname, ext, sep = '-')
		x[[ID1]] = FUN1(x[[ID1]])
		x[-ID1] = lapply(x[-ID1], FUN2)
		plotMap(x, fname, ...)
	}
	plotStuff(1, 'mean', mean, mean)
	#mapply(plotStuff, 1:4, paste('max', 1:4), MoreArgs = list( function(i) max(i),  function(i) min(i)))
	#mapply(plotStuff, 1:4, paste('min', 1:4), MoreArgs = list( function(i) min(i),  function(i) max(i)))

    }
    
    fname = paste0(pr_dataset, '-', drought_var, '-')
    plotMaps(mlim, paste0(fname, 'standard'))
    #plotMaps(nlim, paste0(fname, 'potential'))
    plotMaps(sen, paste0(fname, 'sensitivity'), TRUE)
	
}

#plot4pr_droughts('MSWEP', 'MADD')
runAll_pr_droughts(plot4pr_droughts)

