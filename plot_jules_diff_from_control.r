##################
## cfg			##
##################
source("cfg.r")

limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")

items = c(2:3, 5:6)
##################
## openStuff    ##
##################

control = openJulesTree(Jules_fire_on_LU_on_fname)
fireOff = openJulesTree(Jules_fire_off_LU_on_fname)
LUoff   = openJulesTree(Jules_fire_on_LU_off_fname)
bothOff = openJulesTree(Jules_fire_off_LU_off_fname)
mortExp = lapply(Jules_fire_on_LU_on_fnames, openJulesTree)


mortExpD = mortExp
mortExpD[[1]] = mortExp[[1]] - fireOff
mortExpD[-1] = lapply(mortExp[-1], function(i) i - fireOff)

dev.new()
par(mfrow = c(3,2), mar = rep(0,4))
plotStandardMap(LUoff, limits =  limits/100, cols =  cols, '')
plot.new()
lapply(mortExpD, plotStandardMap, limits =  dlimits/100, cols =  dcols, '')


fireOff = control - fireOff
LUoff = control - LUoff

dev.new()
par(mfrow = c(2,2), mar = rep(0,4))
plotStandardMap(control, limits =  limits/100, cols =  cols, '')
plot.new()
plotStandardMap(fireOff, limits = dlimits/100, cols = dcols, '')
plotStandardMap(  LUoff, limits = dlimits/100, cols = dcols, '')
