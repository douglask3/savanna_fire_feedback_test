library(raster)
library(gitBasedProjects)
library(rasterPlot)
library(rasterExtras)
source("libs/return_multiple_from_functions.r")

setupProjectStructure()
data_dir = 'data/driving_Data/'

fnames_xy = c("MAP (mm/yr)"    = "MAP_MSWEP.nc",
	      "Tree cover (%)" = "TreeCover.nc")
			  
xyscale   = c(1, 100) 

fnames_z  = c("None"                         = "TreeCover.nc",
              "Temperature (~DEG~C)"         = "MAT.nc",
              "Annual burnt area (%)"        = "BurntArea.nc",
              "Rainfall distribution"        = "MADD_MSWEP.nc",
              "Max. Temperature (~DEG~C)"    = "MTWM.nc",
	      "Population density (/k~m2~)"  = "PopDen.nc",
	      "Urban area (%)"               = "urban.nc",
	      "Cropland cover (%)"           = "crop.nc",
	      "Pasture cover (%)"            = "pas.nc")

zscale    = c(1, 1, 1200, 1, 1, 1, 100, 100, 100)

tas_colour = c('#5e4fa2', '#66c2a5', '#96A558', '#Ae904b', '#f46d43', '#9e0142')	  
cols      = list(c("black"),
                 tas_colour,
                 c("black", "#999900", "#FF0000"),
                 c('#00000b', '#05A0FF'),
                 tas_colour,
		 c("black", "blue"),
		 c("black", "purple"),
		 c("black", "green"),
		 c("black", "brown"))

limits  = list(None    = NaN,
               tas     = c(22, 24, 26, 28),
               fire    = c(0.1, 1, 2, 5, 10, 20, 50),
               drought = seq(0.2, 0.8, 0.2),
               tasmax  = c(26, 28, 30, 32),
	       popdens = c(1, 10, 100, 1000),
	       urban   = c(0.01, 0.1, 1, 10),
	       crop    = c(2, 5, 10, 20, 40),
	       pas     = c(2, 5, 10, 20, 40))
			   
extent = c(-180, 180, -30, 30)

openFiles <- function(fnames, scale) {
	fnames = paste(data_dir, fnames, sep = '/')
	dat = lapply(fnames, function(i) raster(i))
	#dat = lapply(dat, mean)
	
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

    plot(x, y, xlab = '', ylab = '', xlim = c(0, 4000), axes = FALSE, type = 'n')
	
    if (name != "None") {    
	z0 = z
	z = cut_results(z, limits)
    	cols = colsFull =
	    make_col_vector(cols, ncols = length(limits) + 1, whiteAt0 = FALSE)
	 
	cols = make.transparent(cols, 0.99)
	cols = cols[z]
        for (cex in c(1, 0.8, 0.6, 0.5, 0.4, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05, 0.03, 0.02, 0.01))
            points(x, y, col = cols, cex = cex, pch = 20)
    } else {
        mask = x > 0 & y > 0
        x = x[mask]
        y = y[mask]
        cols = blues9[unlist(mapply(rep, 1:9, 9 + (1:9)^3))]
        cols = densCols(x,y, colramp = colorRampPalette(cols))
        points(y~x, col = cols, pch = 20)
    }
    
    if (xaxis) axis(1)
    if (yaxis) axis(2)
    if (name != "None") {
        mtext.units(name)
	
	to = rep('-', length(limits) - 1)
        limits = c('<', limits, '>')
	to = c(' ', to, ' ')
        legend = paste(head(limits, -1), to, limits[-1], sep = '')
	legend('topleft', legend, pch = 18, col = colsFull, cex = 0.8)
    }
}


png('figs/MAP_vs_Tree_vs_vars.png', width = 7, height = 7, units = 'in', res = 300)
    lmat = rbind(c(3:5, 0), c(6:8, 0), c(9:11, 1), c(0, 0, 2, 0))

    layout(lmat, widths = c(1, 1, 1, 0.5), heights = c(1, 1, 1, 0.5))
	par(mar = c(0, 1, 3, 0), oma = c(3.5, 3.5, 0, 0))

	yhist = hist(y[y > 0], 100, plot = FALSE)
	barplot(yhist$density, horiz = TRUE, axes = FALSE)

	xhist = hist(x[x > 0 & x < 4000], 100, plot = FALSE)
	barplot(xhist$density, axes = FALSE, ylim = rev(range(xhist$density)))
        par(mar = c(0, 0, 2, 0))
	mapply(plot_3way, zs, znames, limits, cols, c(F, F, F, F, F, F, T,T, T), 
              c(T, F, F, T, F, F, T, F, F))

	mtext.units(xlab, side = 1, line = -4, outer = TRUE, adj = 1.5/3.5)
	mtext.units(ylab, outer = TRUE, side = 2, line = 2.3, adj = 1.5/3.5)

dev.off()#.gitWatermark()

