##################
## cfg			##
##################
source("cfg.r")
graphics.off()

limits_TC = c(1, 10, 20, 30, 40, 50, 60, 70)
cols_TC   = c("white", "#AAAA00", "#003300")

dlimits_TC = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols_TC   = c("#1A001A", "#330033", "#662366", "#AA78AA", "#eed9ee", "#ffffe5", "#d9f0a3", "#78c679", "#238443", "#004529", "#002211")

limits_BA = c(0.1, 1, 2, 3, 5, 10, 20, 30, 50)
cols_BA   = c('#ffffcc','#fed976','#fd8d3c','#e31a1c','#800026')

dlimits_BA = c(-10, -5, -2, -1, -0.1, -0.01, 0.01, 0.1, 2, 5, 10)
dcols_BA   = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))


items = c(2:3, 5:6)
yrs   = c(2002, 2008, 2014)

JULES_dir = "../jules_outputs/"

JULES_control     =  c("u-bk543", "u-bi607_CropFrag")
JULES_experiments =  list(mortExps    = c("u-bk757", "u-bk809", "u-bk716",
                                          "u-bk807", "u-bk811", "u-bk812", "u-bk812"),
                          INFERNOExps = c("u-bi607_CropFrag", "u-bi607_FUEL_RH",
                                          "u-bi607_FUEL", "u-bi607_RH"))

Experiment_names  = list(c("A  MAP", "B  Burnt area", "C  Rainfall distribution",
                           "D  Temperature stress", "G  Cropland", "H  Pasture",
                           "I  All land use"),
                         c("Cropland Fragmentation", "Fuel and RH", "Fuel", "RH"))


lmat = list(rbind(c(0,1), c(2,2), 3:4, 5:6, 0, 7:8,c(9, 0), 10), NULL)
heights = list(c(1, 0.33, 1, 1, 1, 1, 1, 0.33), NULL)
grab_cache = TRUE

######################
## open	            ##
######################	

plotExperimentSet <- function(name, Exp_names, lmat = NULL, heights = 1, normalise = FALSE, ...) {    
    c(Jules_TC_control, Jules_TC_exp, Jules_BA_control, Jules_BA_exp, Jules_dout, Jules_PFTs) :=
        openJulesExperimentSet(name, yrs = yrs, grab_cache = grab_cache,...)
    if (normalise) {
        mortExpD = layer.apply(Jules_TC_exp, function(i) (i - Jules_TC_control)/  max(addLayer(i, Jules_TC_control)))
    } else {
        mortExpD =  (Jules_TC_exp - Jules_TC_control)
    }
    
    mortExpD = lapply(layer.apply(mortExpD, function(i) c(i)), function(i) i[[1]])

    fname = paste0('figs/JULES_mort_maps', '-', name)
    
    if (length(mortExpD) == 1){ nrow = 1; ncol = 1
    } else { ncol = 2; nrow = ceiling(length(mortExpD)/2)}
    
    if (is.null(lmat)) {
        lmat = matrix(c(1:length(mortExpD), 0)[1:(ncol * nrow)], ncol = ncol, nrow =nrow)
        lmat = rbind(c(1, 0), c(2, 0), lmat + 2, max(lmat) + 3)
        heights = c(1, 0.5, rep(1, nrow), 0.5)
    }
    height = (4.75/6.5) * 5 * ((nrow(lmat)-3.5) + 1.6)/4.6
    
    plotMe <- function(fnamei, control, experiment, limits, cols, dlimits, dcols) {
        fname = paste0(fname, '-', fnamei, '.png')
        png(fname, height = height, width = 4.75 * ncol/2, units = 'in', res = 300)
            layout(lmat, heights = heights)
            par(mar = c(0, 0, 0, 0), oma = c(0, 1, 1.5, 1))

            plotStandardMap(control, limits =  limits/100, cols =  cols, 
                            'Control', mtext_line = -.8)
            par(mar = c(0.3, 0, 0,0))
            add_raster_legend2(cols = cols, limits = limits,transpose = FALSE, srt = 0,
                               plot_loc = c(0.15, 0.85, 0.7, 0.9), add = FALSE, 
                               labelss = c(0, limits, 100), units = '%')
            par(mar = rep(0, 4))
            mapply(plotStandardMap, experiment, Exp_names, 
                   MoreArgs = list(limits =  dlimits/100, cols =  dcols, mtext_line = -0.8))
             par(mar = c(0.3, 0, 0,0))
            add_raster_legend2(cols = dcols, limits = dlimits, extend_min = TRUE, extend_max = TRUE,
                               transpose = FALSE, srt = 0, units= '%',
                                plot_loc = c(0.02, 0.98, 0.7, 0.9), add = FALSE)
        dev.off()

    }
    
    plotMe('TC', Jules_TC_control, mortExpD, limits = limits_TC, cols = cols_TC, dlimits = dlimits_TC, dcols = dcols_TC)
    Jules_BA_exp[[2]][] = 0.0
    mortExpD = layers2list(Jules_BA_exp - Jules_BA_control)
    plotMe('BA', Jules_BA_control,  mortExpD,
           limits = limits_BA, cols = cols_BA, dlimits = dlimits_BA, dcols = dcols_BA)
    browser()
}

mapply(plotExperimentSet, paste0(names(JULES_experiments), '-normalise'),
       Experiment_names, JULES_control, JULES_experiments, lmat = lmat, heights = heights, 
       MoreArgs = list(normalise = TRUE))

mapply(plotExperimentSet, names(JULES_experiments), Experiment_names, JULES_control, JULES_experiments, lmat = lmat, heights = heights, 
       MoreArgs = list(normalise = FALSE))
