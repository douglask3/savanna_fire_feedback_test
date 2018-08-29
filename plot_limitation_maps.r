########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")
items = c(2:3, 5:6)
########################################
## load and analyes  			      ##
########################################

out = makeOrLoadEnsembles()
lim = lapply(items, function(i) 1-selectOutput(out, i)[[1]])

sen = lapply(items, function(i)  tail(selectOutput(out, i), 1)[[1]])


limits = c(0.1, 0.25, 0.5)
cols = c("FF", "BB","88", "44", "00")
	
xy = xyFromCell(lim[[1]], 1:length(lim[[1]]))
mlim = lapply(lim, mean)
#mlim[[1 ]][] =  100
nlim = mlim[[1]] + mlim[[2]] + mlim[[3]] + mlim[[4]]
nlim = lapply(mlim, '/', nlim)

sen = lapply(sen, mean)
#nsen = sen[[1]] + sen[[2]] + sen[[3]] + sen[[4]]
#sen = lapply(sen, '/', nsen)

plotMap <- function(x, fname = '', normalise = FALSE) {
	fname = paste('figs/limPlot', fname, '.png', sep = '')
	png(fname, height = 6, width = 12, res = 300, unit = 'in')
	print(fname)
	lmat = rbind(c(1,2),
				 c(3,4),
				 5,
				 6)
	
	layout(lmat, heights = c(1,1,2,0.5))
	par(mar = rep(0,4))
	
	cutQuantuiles <- function(i) {
		lims = quantile(i, c(0.25, 0.5, 0.75))
		cut_results(i, lims)
	}
	
	x = lapply(x, function(i) max.raster(i, na.rm = TRUE) * (i-min.raster(i, na.rm = TRUE))/diff(range.raster(i, na.rm = TRUE)))
	#x = lapply(x, function(i) i[is.na(i)] = 1)
	x[c(1, 3)] = lapply(x[c(1, 3)], function(i) {i[1:20, 345:420][is.na(i[1:20, 345:420])] = max.raster(i, na.rm = TRUE); i})
	x[c(2, 4)] = lapply(x[c(2, 4)], function(i) {i[1:20, 345:420][is.na(i[1:20, 345:420])] = quantile(i, 0.1); i})
	#x = lapply(x, cutQuantuiles)
	#x[[1]][] = 0
	#x[[2]][] = 100
	#x[[3]][] = 0
	plotFun <- function(i, cols) {
		plot_raster_from_raster(i, cols = cols, limits = seq(0.0, 1.0, 0.1), add_legend=FALSE, interior = FALSE)
		addStandardLegend(i, seq(0.0, 1.0, 0.1) , cols, units = '')
	}
	mapply(plotFun, x, 
			cols = list(c("white", '#00FFFF', '#000011'),
			            c("white", '#888800', "#001100"),
						c("white", "#FF00FF", "#110000"),
						c("white", "black")))
	
	
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

plotMap(mlim, 'standard')
plotMap(nlim, 'potential')
plotMap(sen, 'sensitivity', TRUE)