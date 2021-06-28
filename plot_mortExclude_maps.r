########################################
## cfg          		      ##
########################################
source("cfg.r")
graphics.off()

cols = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
limits = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40)

summaryFileC = "model_summary-nEns-20.nc"
summaryFileE = "model_summary-nEns-diff20.nc"
PostDir = "data/sampled_posterior/attempt12"

conID  = "control"
expIDs = c("Burnt area" = "noFire", "Wind" = "noWind",
            "Rainfall\ndistribution" = "noDrought", "Population\ndensity" = "noPop")
obsFile = 'data/driving_Data/TreeCover.nc'   
openDat <- function(id, summaryFile) {
    brick.NaN(paste0(PostDir, '/', id, '/', summaryFile),
              varname = "tree_cover_mean")[[c(3, 7)]]
}

control = 100*openDat(conID, summaryFileC)
obs = raster::resample(raster(obsFile), control)
obs = 100*obs/0.8 
scale = (100-obs)/(100-control)
experiments = lapply(expIDs, openDat, summaryFileE)

plotExp <- function(r, nm) {
    r = (r *scale)*100
    plotStandardMap(r[[1]], cols = cols, limits = limits)
    mtext(side = 2, nm, line = -1.5)
    if (nm == names(experiments)[1]) mtext(side = 3, '10%')
    plotStandardMap(r[[1]], cols = cols, limits = limits)
    if (nm == names(experiments)[1]) mtext(side = 3, '90%')
    diff  <- function(x, w = 1) sum.raster(x * raster::area(x), na.rm = TRUE)/
             sum.raster(w * raster::area(x, na.rm = TRUE), na.rm = TRUE)
    
    diffs = layer.apply(r, diff)
    d = paste0(round(unlist(diffs), 2), collapse = '-', '%')
    text(x = 78, y = -17.5, d, xpd = NA)

    for (i in 1:2) diffs[[i]] = diff(r[[i]], control[[i]] * scale[[i]])*100
    d = paste0(round(unlist(diffs), 2), collapse = '-', '%')
    text(x = 78, y = -27.5, d, xpd = NA)
}

layout(rbind(t(matrix(1:16, nrow = 2)), 17))
par(mar = c(0.5, 0, 0.5, 0), oma = c(0, 2, 2, 0))
mapply(plotExp, experiments, names(experiments))

addStandardLegend(obs, limits, cols, units = '%', srt = 0, add = FALSE,
                  plot_loc = c(0.2, 0.8, 0.73, 0.8), extend_max = TRUE) 


