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
			  
annualAverage <- function(r, ...) {
	
}

annualAverageMax <- function(r, ...) {
	browser()
}

swFromCloud <- function(r, ...) {


}

dryDays <- function(r, ...) {

	browser()
	
}

FUNS = c("TreeCover" = annualAverage, "MAT" = annualAverage, "MTWM" = annualAverageMax, "MAP" = annualAverage,
			  "SW" = swFromCloud, "BurntArea" = annualAverage, "Drought" = dryDays, "PopDen" = annualAverage,
			  "urban" = annualAverage, "crop" = annualAverage, "pas" = annualAverage)
			  
makeAdWriteVar <- function(nme, filename, FUN) {

	browser()
}

mapply(makeAdWriteVar, names(variables), variables, FUNS)

