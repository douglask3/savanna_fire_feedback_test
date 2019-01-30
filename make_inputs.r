source("cfg.r")

"
MADD  = Mean Annual Dry Days
MADDM = Mean Annual Dry Days of the Driest Month
MADM  = Mean Annual Prciep of Dryiest Month
MConc = Mean Annual Seasonal Concentration"

data_dir  = "../LimFIRE/outputs/"
variables = c("TreeCover" = "treecover2000-2014.nc", "MAT" = "Tas2000-2014.nc", "MTWM" = "Tas2000-2014.nc",
			  "SW" = "cld2000-2014.nc", "BurntArea" = "fire2000-2014.nc",
			  "urban" = "urban_area2000-2014.nc", "crop" = "cropland2000-2014.nc", "pas" = "pasture2000-2014.nc", 
			  "PopDen" = "population_density2000-2014.nc", 
			  "MAP_CRU" = "Prc2000-2014.nc", "MADD_CRU" = "Wet2000-2014.nc", "MDDM_CRU" = "Wet2000-2014.nc",
			  "MADM_CRU" = "Prc2000-2014.nc", "MConc_CRU" = "Prc2000-2014.nc")
			  
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

MADD <- function(r, ...) {
	#out = PolarConcentrationAndPhase(r)[[2]]
	out = annualAverageMax(r*(-1))
	out = 1-(out * (-1)/mean(r))
	
	names(out) = "layer"
	return(out)
}

MDDM = function(r, ...) {
	out =  1 + annualAverageMax(r * (-1))
	return(out)
}

MADM = function(r, ...) {
	out = annualAverageMax(r * (-1))
	out = (out/annualAverage(r)) + 1
	return(out)
}

MConc = function(r, ...) {
	print("MConc")
	out = PolarConcentrationAndPhase(r)[[2]]
	return(out)
}

FUNS = c("TreeCover" = annualAverage, "MAT" = annualAverage, "MTWM" = annualAverageMax,
		 "sunshine" = sunshineHours, "BurntArea" = annualAverage12, 
		 "urban" = annualAverage, "crop" = annualAverage, "pas" = annualAverage,"PopDen" = annualAverage, 
		 "MAP_CRU" = annualAverage12, "MADD_CRU" = MADD, "MDDM_CRU" = MDDM,
		 "MADM_CRU" = MADM, "MConc_CRU" = MConc)
			  
			  
scaling = c("TreeCover" = 1/100, "MAT" = 1, "MTWM" = 1,
			  "sunshine" = 1, "BurntArea" = 1,
			  "urban" = 1/100, "crop" = 1/100, "pas" = 1/100, "PopDen" = 1,
			  "MAP_CRU" = 1, "MADD_CRU" = 1, "MDDM_CRU" = 1,
		      "MADM_CRU" = 1, "MConc_CRU" = 1)
			  
MinPoint = c("TreeCover" = 0, "MAT" = 0, "MTWM" = 0,
			  "sunshine" = 0, "BurntArea" = 0,
			  "urban" = 0, "crop" = 0, "pas" = 0, "PopDen" = 0,
			  "MAP_CRU" = 0, "MADD_CRU" = 1, "MDDM_CRU" = 1,
		      "MADM_CRU" = 1, "MConc_CRU" = 1)
			  
makeVar <- function(filename, FUN) {
	print(filename)
	r = brick(paste(data_dir, filename, sep = '/'))
	r = FUN(r)
	return(r)
}

ins = mapply(makeVar, variables, FUNS)

mask = sum(is.na(layer.apply(ins, function(i) layer.apply(i, function(j) j))), na.rm = TRUE)
mask = mask > 3
writeVar <- function(nme, r, sc, mp = 0.0) {
	
	writeSub <- function(nmei, ri) {
		ri[mask] = NaN
		ri[!mask & is.na(ri)] = mp
		names(ri) = NULL
		fname = paste('data/', nmei, '.nc', sep = '')
		print(fname)
		ri =  crop(ri, extent(extent))
		ri = ri * sc
		ri = writeRaster.gitInfo(ri, fname, zname = 'layer', overwrite = TRUE)
		return(ri)
	}
	if (is.raster(r)) r = writeSub(nme, r)
	else {
		nme = paste(nme, 1:length(r), sep = '')
		r = mapply(writeSub, nme, r)
	}
	return(r)
}

mapply(writeVar, names(variables), ins, scaling, MinPoint)




