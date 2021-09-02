source("cfg.r")
graphics.off()

dir = "data/driving_Data/TROPICS/"
files = c("TreeCover.nc", "", "",
          "BurntArea_GFED_four_s.nc", "MADD_MSWEP.nc", "MAP_MSWEP.nc",
          "MaxWind.nc", "MTWM.nc", "MAT.nc",    
          "crop.nc", "pas.nc", "SW1.nc",
          "urban.nc", "PopDen.nc", "SW2.nc")

plot_vars = c("Tree cover", "NaN" = "None", "NaN" = "None",
              "Burnt area", "Rainfall distribution", "MAP",
              "Max. Wind", "MxMTWM", "MAT",
              "Cropland area", "Pasture area", "Direct SW", 
              "Urban area", "Pop. Density", "Defuse SW")

units   = list('%', '', '',
               '%', '', 'mm/yr',
               'm/s', '~DEG~C', '~DEG~C', 
               '%', '%', 'w/~m2~',
               "%", 'people/~km2~', 'w/~m2~')

scaling = list(100/0.8, NaN, NaN,
               100, 1, 1,#exp, 
               1, 1, 1,
               100, 100, 1, 
               100, 1, 1)
pr_limits      = c(100, 500, 1000, 1500, 2000, 2500, 3000, 3500)
drought_limits = seq(0.1, 0.9, 0.1)
limits = list(c(10, 20, 30, 40, 50, 60, 70, 80, 90), NaN, NaN,
              c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50),
              drought_limits,
              pr_limits,
              c(1, 2, 3, 4, 6, 8, 10, 12),
              c(16, 20, 24, 28, 32, 36, 40), 
              c(12, 14, 16, 18, 20, 22, 24, 26, 28),
              seq(10, 90, 10),
              seq(10, 90, 10),
              c(50, 100, 150, 200, 250, 300, 350),
              c(0.0001, 0.001, 0.01,  0.1,  1),
              c(0.1, 1, 10, 100, 1000),
              c(50, 100, 150, 200, 250, 300, 350))
maxLabs = list(100, NULL, NULL,
               100, 1, NULL, 
               NULL, NULL, NULL,
               100, 100, NULL, 
               100, NULL, NULL)
extend_max = list(F, F, F,
                  F, F, T, 
                  T, T, T,
                  F, F, T,
                  F, T, T)
extend_min = list(F, F, F,
                  F, F, F,       
                  F, T, T,
                  F, F, F,
                  F, F, F) 


tas_colour = c('#5e4fa2', '#66c2a5', '#e6f598', '#fee08b', '#f46d43', '#9e0142')
agr_colour = c('#fff7f3', '#fcc5c0', '#f768a1', '#ae017e', '#49006a', '#120017')
prc_colour = c('#ffffd9', '#c7e9b4', '#41b6c4', '#225ea8', '#081d58', '#020d3a')
drt_colour = c('#ffffe5', '#fee391', '#fe9929', '#cc4c02', '#662506', '#160601')
swx_colour = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c',
               '#cb181d','#a50f15','#67000d')

cols = list(c("white" , "#AAAA00", "#003300"),
            'white',
            'white',
            c('#ffffcc', '#fed976', '#fd8d3c', '#e31a1c', '#800026', '#200008'),
            drt_colour,
            prc_colour,
            c('#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf',
              '#3690c0','#02818a','#016c59','#014636'),
            tas_colour,
            tas_colour,
            agr_colour,
            agr_colour,
            swx_colour,
            c('white', 'black'),
            c('#f7fcfd', '#bfb3e6', '#8c96c6', '#88419d', '#4d004b', '#130012'),
            swx_colour)

box = list(NULL, NULL, NULL,          
          c('#AA0000' = 1), c('#AA0000' = 1), c('#0000AA' = 1), 
          c('#AA0000' = 1), c('#AA0000' = 1), c('#FFFF00' = 1), 
          c('black'   = 1), c('black'   = 1), c('#FFFF00' = 1),
          c('black'   = 1), c('black'   = 1), c('#FFFF00' = 1))                

dontOpenBlank <- function(file) {
    if (file == '') return(NULL)
    else return(raster(paste0(dir, file)))
}
dats = lapply(files, dontOpenBlank)


plot_inputs_figure <- function(..., mtext_line = -0.1, y_range = c(-39, 30)) {
    selectNotNone <- function(r, x) {
        select <- function(i) 
            if (i == "None") return(i) else return(r[[i]])
        
        r = lapply(x, select)
        return(r)
    }

    scaleFun <- function(x, i)
        if (is.function(i)) return(i(x)) else if(!is.na(i)) return(x * i) else return(x)

    dat = dats#selectNotNone(dats, plot_vars)
    mask = any(is.na(layer.apply(dat, function(i) i)))
    dat = lapply(dat, function(r) {if(is.null(r)) return(r); r[mask] = NaN; r})
    dat = mapply(scaleFun, dat, scaling)
    
    plotMap <- function(x, box, ...) {
        if (is.raster(x)) {
            
            plotStandardMap(x, ..., y_range = y_range, mtext_line = mtext_line,
                            plot_loc = c(0.05, 0.95, 0.04, 0.11))

            if(is.null(box)) return()
            
            boxFun <- function(...) 
                lines(c(-120, 160, 160, -120, -120), c(-30, -30, 47, 47, -30), 
                      xpd = NA, lwd = 2,  ...)
            mapply(boxFun, col = names(box), lty = box) 

        } else plot.new()
    }
    print(units)
    mapply(plotMap, dat, box, limits = limits, cols = cols, 
           plot_vars, units = units, 
           maxLab = maxLabs, extend_max = extend_max, extend_min = extend_min,
             MoreArgs = list(...))
    
}


###############
## figure S1 ##
###############


#png('figs/inputs_map_new.png', height = 2.375*2*5/4, width = 4.75*2,
#    units = 'in', res = 300)
#    par(mfrow = c(5, 3), mar = c(0, 0.1, 2.5, 0.1), oma = c(2, 0, 0, 2))
#    plot_inputs_figure(add_legend = TRUE)
#dev.off()#.gitWatermark()

###############
## figure S2 ##
###############
drought_vars = c("MADD", "MADM", "MConc", "MDDM")
pr_datasets  = c("CMORPH", "CRU", "GPCC", "MSWEP")
plot_vars    = paste0(rep(c("MAP", drought_vars), each = 4), '_', pr_datasets, ".nc")

#names(plot_vars) = paste(LETTERS[1:20], '', pr_datasets, rep(c("MAP", drought_vars), each = 4))
dats = lapply(plot_vars, dontOpenBlank)
plot_vars = gsub('_', ' ', substr(plot_vars, 1, nchar(plot_vars)-3))
units   = c(rep('mm/yr', 4), rep('', 16))

scaling = c(rep(1, 4), rep(1, 16))

limits  = c(rep(list(pr_limits), 4), rep(list(drought_limits), 16))
maxLabs = c(rep(list(NULL), 4), rep(list(1), 16))
extend_max = c(rep(T, 4), rep(F, 16))
extend_min = FALSE

cols = c(rep(list(prc_colour), 4), rep(list(drt_colour), 16))

box = rep(list(NULL), 16)

png('figs/inputs_pr_dr_map.png', height = 2.2*2, width = 4.75*2, units = 'in', res = 300)
    layout(rbind(1:4, 21, 5:8, 9:12, 13:16, 17:20, 22), heights = c(1, 0.4, 1, 1, 1, 1, 0.4))
    par( mar = c(0, 0, 1, 0), oma = c(0, 0, 1, 0))
    plot_inputs_figure(mtext_line = -0.1, y_range = c(-30, 30)) 
    par(mar = c(0, 0, 0, 0))
    plot.new()
    addStandardLegend(dats[[plot_vars[1]]], pr_limits, prc_colour, units = 'mm/yr', 
                      extend_max = TRUE, labelss = c(0, pr_limits), 
                     plot_loc = c(0.32, 0.67, 0.77, 0.92), srt = 0) 

    plot.new() 
    addStandardLegend(dats[[plot_vars[5]]], drought_limits, drt_colour, units = '',
                maxLab = 1, plot_loc = c(0.32, 0.67, 0.77, 0.92), srt = 0)
dev.off()#.gitWatermark()
