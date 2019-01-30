library(raster)
library(gitBasedProjects)
library(rasterPlot)
library(rasterExtras)
source("libs/return_multiple_from_functions.r")

setupProjectStructure()
data_dir = '../LimFIRE/outputs/'

fnames_xy = c("MAP (mm/yr)" = "Prc2000-2014.nc",
			  "Tree Cover (%)"   = "treecover2000-2014.nc")
			  
xyscale   = c(12, 1) 
			  
fnames_z  = c("Annual Burnt Area (%)" = "fire2000-2014.nc", 
			  "Population Density (/k~m2~)" 
									  = "population_density2000-2014.nc",
			  "Cropland Cover (%)"    = "cropland2000-2014.nc",
			  "Pasture Cover (%)"     = "pasture2000-2014.nc",
			  "Urban Area (%)"        = "urban_area2000-2014.nc")

zscale    = c(1200, 1, 1, 1, 1)
			  
cols      = list(c("black", "#999900", "#FF0000"),
				 c("black", "purple"),
				 c("black", "green"),
				 c("black", "brown"),
				 c("black", "blue"))

limits  = list(fire    = c(0.1, 1, 2, 5, 10, 20, 50),
			   popdens = c(1, 10, 100, 1000),
			   crop    = c(2, 5, 10, 20, 40, 60, 80),
			   pas     = c(2, 5, 10, 20, 40, 60, 80),
			   urban   = c(0.01, 0.1, 1, 10))
			   
extent = c(-180, 180, -30, 30)

openFiles <- function(fnames, scale) {
	fnames = paste(data_dir, fnames, sep = '/')
	dat = lapply(fnames, function(i) brick(i)[[1:12]])
	dat = lapply(dat, mean)
	
	dat = lapply(dat, raster::crop, extent(extent))
	dat = mapply('*', dat, scale)
	return(dat)	
}

c(x, y) := openFiles(fnames_xy, xyscale)
y[is.na(x)] = NaN
c(xlab, ylab) := names(fnames_xy)

zs = openFiles(fnames_z, zscale)
znames = names(fnames_z)

plot_3way <- function(z, name, limits, cols, xaxis, yaxis) {
	mask = !is.na(x + y + z)
	x = x[mask]
	y = y[mask]
	z = z[mask]
	
	z0 = z
	z = cut_results(z, limits)
	cols = colsFull =
		make_col_vector(cols, ncols = length(limits) + 1, whiteAt0 = FALSE)
	 
	cols = make.transparent(cols, 0.9)
	z = cols[z]
	
	
	plot(x, y,
		 cex = 0.5, pch = 19, col = z,
		 xlab = '', ylab = '', xlim = c(0, 4000), axes = FALSE)
	
	if (xaxis) axis(1)
	if (yaxis) axis(2)
	mtext.units(name)
	
	to = rep('-', length(limits) - 1)
	limits = c('<', limits, '>')
	to = c(' ', to, ' ')
	legend = paste(head(limits, -1), to, limits[-1], sep = '')
	legend('topleft', legend, pch = 18, col = colsFull)
	
	
}

plot_all <- function() {
    graphics.off()
    png('MAP_vs_Tree_vs_vars.png', width = 7, height = 7, units = 'in', res = 300)
    	lmat = rbind(c(3:4, 1), c(5:6, 0), c(7, 2, 0), c(7, 0, 0))
	layout(lmat, widths = c(1, 1, 0.5), heights = c(1, 1, 0.5, 0.5))
	par(mar = c(0, 0, 2, 0), oma = c(3.5, 3.5, 0, 0))

	yhist = hist(y[y > 0], 100, plot = FALSE)
	barplot(yhist$density, horiz = TRUE, axes = FALSE)

	xhist = hist(x[x > 0 & x < 4000], 100, plot = FALSE)
	barplot(xhist$density, axes = FALSE, ylim = rev(range(xhist$density)))

	mapply(plot_3way, zs, znames, limits, cols, c(F, F, F, T, T), c(T, F, T, F, T))

	mtext.units(xlab, side = 1, line = 2.3)
	mtext.units(ylab, outer = TRUE, side = 2, line = 2.3)
    dev.off()
}


