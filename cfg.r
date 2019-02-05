
library(benchmarkMetrics)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs('../gitProjectExtras/gitBasedProjects/R/')
source('../gitProjectExtras/package_git2r.r')
config(repository(), user.name="Douglas Kelley", user.email="douglas.i.kelley@gmail.com")

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
library(MASS)
data(worldHiresMapEnv)

sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')
sourceAllLibs('../rasterextrafuns/rasterExtras/R/')

setupProjectStructure(dirn = c("outputs", "data", "temp", "figs"))
ens_dir = paste(outputs_dir, '/ensembles_noSW/', sep = '')
makeDir(ens_dir)

ensemble_no = round(seq(1,4200, length.out = 6) )
grab_cache_default = TRUE

sourceAllLibs('libs/')
sourceAllLibs('libs/LimTREE_r/')
sourceAllLibs('libs/plotting/')

paramFile = 'outputs/params.csv'

extent = c(-180, 180, -30, 30)

plot_title = c('MAP', 'MAT', 'Disturbance', 'Exclusion')

Jules_fire_off_LU_off_fname = '../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-S2/'
Jules_fire_off_LU_on_fname = '../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF3-mort0/'
Jules_fire_on_LU_off_fname = '../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF3/'
Jules_fire_on_LU_on_fname = '../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF3-2/'

Jules_fire_on_LU_on_fnames = c(Jules_fire_on_LU_on_fname, 
							   "../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF3-mortHlf/",
							   "../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF3-mort10th/",
							   "../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF3-mort100th/",
							   "../fireMIPbenchmarking/data/ModelOutputs/JULES-INFERNO-SF3-fracHalf/")