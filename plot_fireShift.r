source("cfg.r")
graphics.off()
ba_dir = "data/driving_Data/TROPICS/"
ba_pattern = "BurntArea_"
period_pattern = c("1st", "2nd")

fire_limits = c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50)
fire_dlimits = c(-10, -5, -2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2, 5, 10)
fire_cols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
fire_dcols = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))

impact_cols = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929',
         '#ec7014','#cc4c02','#993404','#662506')

impact_limits = c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2)

summaryFileC = "model_summary-nEns-11.nc"
summaryFileE = "model_summary-nEns-diff11.nc"
PostDir = "data/sampled_posterior/attempt15/"

expID = "secondFire"

tree_dcols = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
tree_dlimits = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40)
tree_dlimits = c(rev(-tree_dlimits), tree_dlimits)/10

##########
## FUNS ##
##########
legFun <- function(r, limits, cols, plotLSc = 2/3, ...)
    addStandardLegend(r, limits, cols, '%', add = FALSE,
                      srt = 0, plot_loc = c(0.1*plotLSc, 0.9*plotLSc, 0.6, 0.8), ...) 

##########################
## plus0.01 experiment ##
#########################

ba = brick.NaN("data/sampled_posterior/attempt15/firePlus1pc/model_summary-nEns-diff11.nc", 
               varname = "tree_cover_mean", layers = c(3, 7))

png("figs/baPlus0.01_map.png", height = 3.67, width = 5, res = 300, units = 'in')
    layout(rbind(1, 2, 3, 4), heights = c(1, 0.3, 1, 0.3))
    par(mar = rep(0.75, 4), oma = c(0, 0, 1, 0))
    plotStandardMap(-ba[[1]]*100, '90%',limits = impact_limits, cols = impact_cols)
    par(mar = c(0.5, 0, 0, 0))
    legFun(ba[[1]]*100, impact_limits, impact_cols, 1, extend_max = TRUE)

    par(mar = rep(0.75, 4))
    plotStandardMap(-ba[[2]]*100, '10%' ,limits = impact_limits/20, cols = impact_cols)
    par(mar = c(0.5, 0, 0, 0))
    legFun(ba[[1]]*100, impact_limits/20, impact_cols, 1, extend_max = TRUE)
dev.off()
browser()

openMod <- function(id, summaryFile = summaryFileE) 
    brick.NaN(paste0(PostDir, '/', id, '/', summaryFile),
              varname = "tree_cover_mean", layers = c(3, 5, 7))
mod = openMod(expID) 
ctr = openMod("control", summaryFileC)
obs = raster("data/driving_Data/TROPICS/TreeCover.nc")

openObs <- function(pattern, not = FALSE) {
    files = list.files(ba_dir, full.names = TRUE)
    test = grepl(pattern, files)
    if (not) test = !test
    
    files = files[grepl(ba_pattern, files) & !grepl('4', files) & test]
    
    out = lapply(files, raster)
    out = layer.apply(out, function(i) raster::resample(i, mod[[1]]))
    names(out) = c('GFED4s', 'GFED4', 'MCD45', 'Fire_CCI5.1', 'Fire_CCI54.0')
    out
}


ba  = openObs("Half", TRUE)
baar = layer.apply(ba*raster::area(ba[[2]], na.rm = TRUE)/sum.raster(raster::area(ba[[2]], na.rm = TRUE), na.rm = TRUE), sum.raster, na.rm = TRUE)
print(range(sapply(baar, '*', 100)))
png("figs/burnt_area_annualAverager.png", height = 3.5, width = 7.2, res = 300, units = 'in')
    layout(rbind(1:2, 3:4, 5:6, 7), heights = c(1, 1, 1, 0.5))
    par(mar = rep(0.75,4), oma = c(0, 0, 1, 0))
    titles = c('GFED4s', 'GFED4', 'MCD45', 'Fire_CCI5.1', 'Fire_CCI4.0')
    mapply(plotStandardMap, layers2list(ba*100), titles,
           MoreArgs = list(limits = fire_limits, cols = fire_cols) )
    plot.new()
    par(mar = rep(0, 4))
    legFun(ba[[1]]*100, fire_limits, fire_cols, 1, extend_max = TRUE)
dev.off()

bas = lapply(period_pattern, openObs)

dba = range(bas[[2]]-bas[[1]])
dbaS = dba[[1]]
dbaS[] = NaN
dbaS[dba[[1]] > 0] =  1
dbaS[dba[[2]] < 0] = -1


if (T) {
png("figs/treeChange2Fire.png", width = 9, height = 3, units = 'in', res = 300)
    layout(rbind(c(1:2, 0), 3, 4:6, 7, c(8:10), 11), heights = c(1, 0.5, 1, 0.5, 1, 0.5))
    par(mar = rep(0, 4))

    layer.apply(ba*100, plotStandardMap, fire_limits, fire_cols) 
    legFun(ba[[1]]*100, fire_limits, fire_cols, extend_max = TRUE)

    layer.apply(dba*100, plotStandardMap, fire_dlimits, fire_dcols) 
    layer.apply(dbaS, plotStandardMap, c(-0.5, 0.5), c('blue', 'white', 'red')) 

    legFun(dba[[1]]*100, fire_dlimits, fire_dcols, extend_max = TRUE, extend_min = TRUE)
    legend('right', pch = 15, col = c('blue', 'red'),   
           legend = c('consistent decrease', 'consistent increase'), horiz = TRUE)

    layer.apply(mod[[c(3, 1)]]*100, plotStandardMap, tree_dlimits, tree_dcols) 
    plot.new()
    legFun(mod[[1]]*100, tree_dlimits, tree_dcols, 1, extend_min = TRUE, extend_max = TRUE)


dev.off()
}
#plot(c(0, 100), c(-20, 20), type = 'n')
#
#bins = seq(0, 1, 0.01)

