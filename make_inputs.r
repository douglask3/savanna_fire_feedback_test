library(benchmarkMetrics)
library(gitBasedProjects)
library(raster)
library(ncdf4)
library(rasterExtras)
library(rasterPlot)
library(plotrix)
library(mapdata)
library(mapplots)
library(ellipse)
library(vegan)
library(RcppEigen)
library(parallel)
library(snow)
library(reldist)
data(worldHiresMapEnv)

data_dir  = "../LimFIRE/outputs/"
variables = c("TreeCover" = "treecover2000-2014.nc", "MAT" = "Tas2000-2014.nc", "MTWM" = "Tas2000-2014.nc", "MAP" = "Prc2000-2014.nc",
			  "SW" = "cld2000-2014.nc", "BurntArea" = "fire2000-2014.nc", "Drought" = "Wet2000-2014.nc", "PopDen" = "population_density2000-2014.nc",
			  "urban" = "urban_area2000-2014.nc", "crop" = "cropland2000-2014.nc", "pas" = "pasture2000-2014.nc")
			  
annualAverage <- function(...) mean(...)

annualAverage12 <- function(...) 12 * annualAverage(...)

annualAverageMax <- function(r, ...) {
	nyr = nlayers(r)/12
	annualMax <- function(yr) {
		mn = ((yr-1)*12 + 1):(yr*12)
		return(max(r[[mn]]))
	}
	
	ra = layer.apply(1:nyr, annualMax)
	return(mean(ra))
}

sunshineHours <- function(r, Q00 = 1360, ...) {
	midDay = 2 * pi * seq(15, 345, 30)/360
	Q0 = Q00 * (1 + 2 * 0.01675 * cos(midDay))
	
	lat = r[[1]]
	lat[] = yFromCell(lat, 1:length(lat))
	lat = 2 * lat * pi / 360
	
	delta = -23.4 * cos(midDay + pi * 2 * 10/360) * 2 * pi / 360
	
	cz = layer.apply(sin(delta) , function(i) i * sin(lat)) + layer.apply(cos(delta) , function(i) i * cos(lat))/pi
	cz[cz < 0] = 0
	cz = cz * Q0
	
	SW1 = mean(cz)
	SW2 = mean(cz * (1 - r/100))
	return(list(SW1, SW2))
}

dryDays <- function(r, ...) 30 * annualAverageMax(1-r)

FUNS = c("TreeCover" = annualAverage, "MAT" = annualAverage, "MTWM" = annualAverageMax, "MAP" = annualAverage12,
			  "sunshine" = sunshineHours, "BurntArea" = annualAverage12, "Drought" = dryDays, "PopDen" = annualAverage,
			  "urban" = annualAverage, "crop" = annualAverage, "pas" = annualAverage)
			  
makeVar <- function(filename, FUN) {
	r = brick(paste(data_dir, filename, sep = '/'))
	r = FUN(r)
	return(r)
}

ins = mapply(makeVar, variables, FUNS)

mask = is.na(sum(layer.apply(ins, function(i) layer.apply(i, function(j) j))))

writeVar <- function(nme, r) {
	writeSub <- function(nmei, ri) {
		ri[mask] = NaN
		fname = paste('data/', nmei, '.nc', sep = '')
		ri =  crop(ri, extent(c(-180, 180, -30, 30)))
		ri = writeRaster(ri, fname, overwrite = TRUE)
		return(ri)
	}
	if (is.raster(r)) r = writeSub(nme, r)
	else {
		nme = paste(nme, 1:length(r), sep = '')
		r = mapply(writeSub, nme, r)
	}
	return(r)
}

mapply(writeVar, names(variables), ins)




