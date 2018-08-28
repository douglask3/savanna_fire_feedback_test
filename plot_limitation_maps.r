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
cols = c("FF", "CC", "99", "55", "11")
	
xy = xyFromCell(lim[[1]], 1:length(lim[[1]]))
mlim = lapply(lim, mean)
#mlim[[3 ]][] =  100
nlim = mlim[[1]] + mlim[[2]] + mlim[[3]] + mlim[[4]]
nlim = lapply(mlim, '/', nlim)

sen = lapply(sen, mean)
#nsen = sen[[1]] + sen[[2]] + sen[[3]] + sen[[4]]
#sen = lapply(sen, '/', nsen)

plotMap <- function(x, normalise = FALSE) {
	pout = lapply(x, values)

	if (normalise) cols = rev(cols)
	plot_4way(xy[,1], xy[,2], pout[[1]], pout[[3]], pout[[2]], pout[[4]],
			  x_range = c(-180, 180), y_range = c(-30, 30),
			  cols = 	rev(cols), limits = limits, 
			  coast.lwd=par("lwd"),ePatternRes = 40, ePatternThick = 0.5,
			  add_legend=FALSE, smooth_image=FALSE,smooth_factor=5, normalise = normalise)

			  
	#par(mar = c(3, 10, 0, 8))
	#add_raster_4way_legend(cols = cols ,
	#				   labs = c('<- Moisture', 'Fuel ->', 'Igntions ->', 'Land Use'))
}
par(mfrow = c(3,1), mar = rep(0,4))
plotMap(mlim)
plotMap(nlim)
plotMap(sen, TRUE)