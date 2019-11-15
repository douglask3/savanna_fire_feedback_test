source("make_precip_inputs.r")

"
MADD  = Mean Annual Dry Days
MADDM = Mean Annual Dry Days of the Driest Month
MADM  = Mean Annual Prciep of Dryiest Month
MConc = Mean Annual Seasonal Concentration
"

data_dir  = "../LimFIRE/outputs/"
variables = c("TreeCover" = "treecover2000-2014.nc",
              "MaxWind" = "../../savanna_fire_feedback_test/data/CRUNCEP.wspeed.r0d5.1997.2013.nc",
              "MAT" = "Tas2000-2014.nc",
              "MTWM" = "../data/cru_ts4.03/cru_ts4.03.1901.2018.tmx.dat.nc",
              "MTCM" = "../data/cru_ts4.03/cru_ts4.03.1901.2018.tmn.dat.nc",
	      "SW" = "cld2000-2014.nc",
              "BurntArea_GFED_four_s" = "fire2000-2014.nc",
              "BurntArea_GFED_four" = "../../fireMIPbenchmarking/data/benchmarkData/GFED4.nc",
              "BurntArea_meris" = "../../fireMIPbenchmarking/data/benchmarkData/meris_v2.nc",
              "BurntArea_MODIS" = "../../fireMIPbenchmarking/data/benchmarkData/MODIS250_q_BA_regridded0.5.nc",
              "BurntArea_MCD_forty_five" = "../../fireMIPbenchmarking/data/benchmarkData/MCD45.nc",
	      "urban" = "urban_area2000-2014.nc", "crop" = "cropland2000-2014.nc",
              "pas" = "pasture2000-2014.nc", 
	      "PopDen" = "population_density2000-2014.nc", 
	      "MAP_CRU" = "Prc2000-2014.nc",
              "MADD_CRU" = "Wet2000-2014.nc", "MDDM_CRU" = "Wet2000-2014.nc",
	      "MADM_CRU" = "Prc2000-2014.nc", "MConc_CRU" = "Prc2000-2014.nc",
              "buffalo" = "../../savanna_fire_feedback_test/data/livestock/buffaloLivestock0.5.nc",
              "cattle" = "../../savanna_fire_feedback_test/data/livestock/cattleLivestock0.5.nc",
              "goat" = "../../savanna_fire_feedback_test/data/livestock/goatLivestock0.5.nc",
              "sheep" = "../../savanna_fire_feedback_test/data/livestock/sheepLivestock0.5.nc")
			  
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

temp_max <- function(r, ...) 
   annualAverageMax(r[[1195:1362]])


temp_min <- function(r, ...)
    annualAverageMax(r[[1195:1362]]*(-1))*(-1)


AllMax <- function(r, ...) r = max(r)

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
	
        r = r / 100
	SW2 = mean(cz * r) # defuse
	SW1 = mean(cz * (1 - r)) # direct
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

makeWind <- function(r, ...) 
    max(r[[55:198]])

layer1GT0 <- function(r, ...) {
    r = r[[1]]
    r[r<0] = 0.0
    return(r)
}

FUNS = c("TreeCover" = annualAverage, "MaxWind" = makeWind,
         "MAT" = annualAverage,
         "MTWM" = temp_max,
         "MTCM" = temp_min, 
	 "sunshine" = sunshineHours,
         "BurntArea_GFED_four_s" = annualAverage12,
         "BurntArea_GFED_four"  = annualAverage12,
         "BurntArea_meris"  = annualAverage12,
         "BurntArea_MODIS"  = annualAverage12,
         "BurntArea_MCD_forty_five"  = annualAverage12,
	 "urban" = annualAverage, "crop" = annualAverage, "pas" = annualAverage,
         "PopDen" = annualAverage, 
	 "MAP_CRU" = annualAverage12, "MADD_CRU" = MADD, "MDDM_CRU" = MDDM,
	 "MADM_CRU" = MADM, "MConc_CRU" = MConc,
         "buffalo" = layer1GT0, "goat" = layer1GT0, "cattle" = layer1GT0, "sheep" = layer1GT0)
			  
			  
scaling = c("TreeCover" = 1, "MaxWind" = 1, "MAT" = 1, "MTWM" = 1,
            "MTCM" = 1,
	    "sunshine" = 1,
            "BurntArea_GFED_four_s" = 1,
            "BurntArea_GFED_four"  = 1,
            "BurntArea_meris"  = 1,
            "BurntArea_MODIS"  = 1,
            "BurntArea_MCD_forty_five"  = 1,
	    "urban" = 1, "crop" = 1, "pas" = 1, "PopDen" = 1,
	    "MAP_CRU" = 1, "MADD_CRU" = 1, "MDDM_CRU" = 1,
	    "MADM_CRU" = 1, "MConc_CRU" = 1,
            "buffalo" = 1, "goat" = 1, "cattle" = 1, "sheep" = 1)
			  
MinPoint = c("TreeCover" = 0, "MaxWind" = 0, "MAT" = 0, "MTWM" = 0,
             "MTCM" = 0,
	     "sunshine" = 0,
             "BurntArea_GFED_four_s" = 0,
             "BurntArea_GFED_four"  = 0,
             "BurntArea_meris"  = 0,
             "BurntArea_MODIS"  = 0,
             "BurntArea_MCD_forty_five"  = 0,
	     "urban" = 0, "crop" = 0, "pas" = 0, "PopDen" = 0,
	     "MAP_CRU" = 0, "MADD_CRU" = 1, "MDDM_CRU" = 1,
	     "MADM_CRU" = 1, "MConc_CRU" = 1,
             "buffalo" = 0, "goat" = 0, "cattle" = 0, "sheep" = 0)
			  
makeVar <- function(filename, FUN) {
	print(filename)
	r = brick(paste(data_dir, filename, sep = '/'))
	r = FUN(r)
	return(r)
}

ins = mapply(makeVar, variables, FUNS)

ins_all = c(unlist(ins), unlist(pr_ins))

mask = is.na(ins_all[[1]][[1]])

for (i in ins_all[-1]) {
    i0 = i
    mask = raster::crop(mask, i)
    i = raster::crop(i, mask)
    i = raster::resample(i, mask)
    mask = mask + is.na(i)
}
mask = mask > 3    

writeVar <- function(nme, r, sc, mp = 0.0) {
	
	writeSub <- function(nmei, ri) {
                ri = raster::crop(ri, mask)
                ri = raster::resample(ri, mask)
		ri[mask] = NaN
		ri[!mask & is.na(ri)] = mp
		names(ri) = NULL
		fname = paste('data/driving_Data/', nmei, '.nc', sep = '')
		print(fname)
		ri =  crop(ri, extent(extent))
		ri = ri * sc
		ri = writeRaster.gitInfo(ri, fname, zname = 'layer',
                                         comment = list(src_file = 'make_inputs.r'),  
                                         overwrite = TRUE)
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

maskAndReout_pr <- function(r) {
    fname = filename(r)
    r = raster::crop(r, mask)
    r[mask] = NaN
    r = writeRaster.gitInfo(r, fname, zname = 'layer',
                            comment = list(source='Based on data the amazing Li G processed for me. Regridded for 0.25 to 0.5', 
                                           src_file = 'make_inputs.r'), 
                            overwrite = TRUE)
    return(r)
}

dats = lapply(unlist(pr_ins), maskAndReout_pr)
