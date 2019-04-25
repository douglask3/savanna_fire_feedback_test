##################
## cfg			##
##################
source("cfg.r")

limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")

items = c(2:3, 5:6)

JULES_control     =  "data/JULES-mort/mort0/"
JULES_experiments =  paste0("data/JULES-mort/", c("mort1",  "mortc", "mortr", 
                            "respv/experiment", "resp0/experiment"), '/')
Experiment_names  = c("100% mortality", "PFT-specific + crop masking", 
                      "frac_min=0.1", "scaled resp + PFT-specfic mort", "scaled resp 100% mort")

                      
JULES_experiments =  paste0("data/JULES-mort/", c("mort1",  "mortv", "mortc", "mortx"), '/')
Experiment_names  = c("100% mortality", "PFT-specific", "PFT-specfic + crop masking", 
                      "low mort")
######################
## open				##
######################	
dat = loadInputData()

## Inferened
if (file.exists(temp_file) & TRUE) {
	load(temp_file)
} else {
	out = makeOrLoadEnsembles()	
	out = selectOutput(out)

	## Jules
	Jules_TC_fire_off = openJulesTree(JULES_control, yrs = c(2002, 2008, 2014))
	Jules_PFTs = openJulesTree(JULES_control, splitPFTs = TRUE, yrs = c(2002, 2008, 2014))
	Jules_fire = layer.apply(JULES_experiments, openJulesTree,  1, "burnt_area_gb", yrs = c(2002, 2008, 2014)) * 60 * 60 * 24 * 365 # Jules_TC_fire_off = openJulesTree(Jules_fire_off_LU_off_fname)
	Jules_fire = raster::resample(Jules_fire, Jules_TC_fire_off)
	Jules_TC_fire_on = layer.apply(JULES_experiments, openJulesTree, yrs = c(2002, 2008, 2014))
		
    
	Jules_dout = (Jules_TC_fire_off - Jules_TC_fire_on)/max(addLayer(Jules_TC_fire_off, Jules_TC_fire_on))		
	Jules_dout =  squeeze(Jules_dout, 500)
	#Jules_dout[ Jules_dout < treeMin] = treeMin + treeMin/10
	dout = lapply(out, function(i) 1-out[[1]]/i)
	
	save(out, Jules_TC_fire_off, Jules_fire, Jules_TC_fire_on, Jules_dout, dout, Jules_PFTs, file = temp_file)
}



mortExpD =  (Jules_TC_fire_on - Jules_TC_fire_off)
mortExpD = lapply(layer.apply(mortExpD, function(i) c(i)), function(i) i[[1]])
#mortExpD[[1]] = mortExp[[1]] - fireOff
#mortExpD[-1] = lapply(mortExp[-1], function(i) i - fireOff)

graphics.off()
png('figs/JULES_mort_maps.png', height = 5, width = 12, units = 'in', res = 300)
    lmat = rbind(c(1, 0), c(2, 0), c(3, 4), c(5, 6), c(7, 7))
    layout(lmat, heights = c(1, 0.25, 1, 1, 0.25))
    par(mar = c(0, 0, 1, 0), oma = c(0, 1, 1.5, 1))
    plotStandardMap(Jules_TC_fire_off, limits =  limits/100, cols =  cols, 'Control, no fire', mtext_line = -0)
    add_raster_legend2(cols = cols, limits = limits,transpose = FALSE, srt = 0, plot_loc = c(0.2, 0.8, 0.7, 0.89)    , add = FALSE, 
                       labelss = c(0, limits, 100))
    mapply(plotStandardMap, mortExpD, MoreArgs = list(limits =  dlimits/100, cols =  dcols, mtext_line = -0), Experiment_names)
    add_raster_legend2(cols = dcols, limits = dlimits, extend_min = TRUE, extend_max = TRUE, transpose = FALSE, srt = 0, plot_loc = c(0.35, 0.65, 0.7, 0.89), add = FALSE)
dev.off()
    browser()
fireOff = control - fireOff
LUoff = control - LUoff

dev.new()
par(mfrow = c(2,2), mar = rep(0,4))
plotStandardMap(control, limits =  limits/100, cols =  cols, '')
plot.new()
plotStandardMap(fireOff, limits = dlimits/100, cols = dcols, '')
plotStandardMap(  LUoff, limits = dlimits/100, cols = dcols, '')
