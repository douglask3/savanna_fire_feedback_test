##################
## cfg			##
##################
source("cfg.r")
graphics.off()
limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")

dcols = c("#1A001A", "#330033", "#662366", "#AA78AA", "#eed9ee", "#ffffe5", "#d9f0a3", "#78c679", "#238443", "#004529", "#002211")

items = c(2:3, 5:6)
yrs   = c(2002, 2008, 2014)

JULES_dir = "../jules_outputs/"

JULES_control     =  c("u-bk543", "u-bi607_CropFrag")
JULES_experiments =  list(mortExps    = c("u-bk757", "u-bk716"),
                          INFERNOExps = c("u-bi607_CropFrag", "u-bi607_FUEL_RH",
                                          "u-bi607_FUEL", "u-bi607_RH"))

Experiment_names  = list(c("No Rainfall limitation", "No Rainfall distribution"),
                         c("Cropland Fragmentation", "Fuel and RH", "Fuel", "RH"))


######################
## open	            ##
######################	
openJulesExperimentSet <- function(name, JULES_control, JULES_experiments) { 
    temp_file = paste0('temp/plot_jules_diff_from_control_new', name, '.Rd')
    if (file.exists(temp_file) & TRUE) {
	load(temp_file)
    } else {        
	Jules_TC_control = openJulesTree(JULES_control, JULES_dir, yrs = yrs)
	Jules_PFTs       = openJulesTree(JULES_control, JULES_dir, splitPFTs = TRUE, yrs = yrs)
	Jules_fire       = layer.apply(JULES_experiments, openJulesTree, JULES_dir, 1,
                                 "burnt_area_gb", yrs = yrs) *   60 * 60 * 24 * 365 
	Jules_fire       = raster::resample(Jules_fire, Jules_TC_control)
	Jules_TC_exp     = layer.apply(JULES_experiments, openJulesTree, JULES_dir, yrs = yrs)
		
	Jules_dout = (Jules_TC_control - Jules_TC_exp)/Jules_TC_control
	Jules_dout =  squeeze(Jules_dout, 500)
	
	save(Jules_TC_control, Jules_fire, Jules_TC_exp, Jules_dout, 
             Jules_PFTs, file = temp_file)
    }
    return(list(Jules_TC_control, Jules_fire, Jules_TC_exp, Jules_dout))
}

plotExperimentSet <- function(name, Exp_names, ...) {
    
    c(Jules_TC_control, Jules_fire, Jules_TC_exp, Jules_dout) := openJulesExperimentSet(name, ...)
    mortExpD =  (Jules_TC_exp - Jules_TC_control)
    mortExpD = lapply(layer.apply(mortExpD, function(i) c(i)), function(i) i[[1]])

    fname = paste0('figs/JULES_mort_maps', '-', name, '.png')
    
    if (length(mortExpD) == 1){ nrow = 1; ncol = 1
    } else { ncol = 2; nrow = ceiling(length(mortExpD)/2)}
    lmat = matrix(c(1:length(mortExpD), 0)[1:(ncol * nrow)], ncol = ncol, nrow =nrow)
    lmat = rbind(c(1, 0), c(2, 0), lmat + 2, max(lmat) + 3)

    png(fname, height = 1.1 * (nrow + 1.5), width = 3.5 * ncol, units = 'in', res = 300)
        layout(lmat, heights = c(1, 0.5, rep(1, nrow), 0.5))
        par(mar = c(0, 0, 0, 0), oma = c(0, 1, 1.5, 1))

        plotStandardMap(Jules_TC_control, limits =  limits/100, cols =  cols, 
                        'Control', mtext_line = -.33)
        add_raster_legend2(cols = cols, limits = limits,transpose = FALSE, srt = 0,
                           plot_loc = c(0.2, 0.8, 0.8, 0.89), add = FALSE, 
                           labelss = c(0, limits, 100))
        mapply(plotStandardMap, mortExpD, Exp_names, 
               MoreArgs = list(limits =  dlimits/100, cols =  dcols, mtext_line = -0.33))
        add_raster_legend2(cols = dcols, limits = dlimits, extend_min = TRUE, extend_max = TRUE,
                           transpose = FALSE, srt = 0, plot_loc = c(0.2, 0.8, 0.8, 0.89), add = FALSE)
    dev.off()
}

mapply(plotExperimentSet, names(JULES_experiments), Experiment_names, JULES_control, JULES_experiments)
