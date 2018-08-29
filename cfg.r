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

sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')
sourceAllLibs('../rasterextrafuns/rasterExtras/R/')

setupProjectStructure(dirn = c("outputs", "data", "temp", "figs"))
ens_dir = paste(outputs_dir, '/ensembles_noSW/', sep = '')
makeDir(ens_dir)

ensemble_no = round(seq(1,4200, length.out = 11) )

sourceAllLibs('libs/')
sourceAllLibs('libs/LimTREE_r/')
sourceAllLibs('libs/plotting/')

paramFile = 'outputs/params.csv'

extent = c(-180, 180, -30, 30)

plot_title = c('MAP', 'MAT', 'Disturbance', 'Exclusion')