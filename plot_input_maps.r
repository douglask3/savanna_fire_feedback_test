source("cfg.r")
graphics.off()

dats = dats[[1]]

            
plot_inputs_figure <- function(...) {
    selectNotNone <- function(r, x) {
        select <- function(i) 
            if (i == "None") return(i) else return(r[[i]])
        
        r = lapply(x, select)
        return(r)
    }

    scaleFun <- function(x, i)
        if (is.function(i)) return(i(x)) else if(!is.na(i)) return(x * i) else return(x)

    dat = selectNotNone(dats, plot_vars)
    dat = mapply(scaleFun, dat, scaling)
   
    plotMap <- function(x, box, ...) {
        if (is.raster(x)) {
            plotStandardMap(x, ..., mtext_line = -1.5)

            if(is.null(box)) return()
            
            boxFun <- function(...) 
                lines(c(-120, 160, 160, -120, -120), c(-30, -30, 45, 45, -30), 
                      xpd = NA, lwd = 2,  ...)
            mapply(boxFun, col = names(box), lty = box) 

        } else plot.new()
    }
    mapply(plotMap, dat, box, limits = limits, cols = cols, 
           names(plot_vars), units = units, MoreArgs = list(...))
    
}


###############
## figure S1 ##
###############
plot_vars = c("A  Tree cover" = "TreeCover", "B  MAP" = "MAP_MSWEP", "C MAT" = "MAT",
              "D  Burnt area" = "BurntArea", "E  Rainfall distribution" = "MADD_MSWEP", "F  MxMTWM" = "MTWM",
              "G  Population Density" = "PopDen", "H  Urban Area" = "urban", "I  Cropland Area" = "crop",
              "NaN" = "None", "NaN" = "None", "J  Pasture Area" = "pas")

units   = list('%', 'mm/yr', '~DEG~C', '%', '', '~DEG~C',
               "people/k~m2~", '%', '%', '', '', '%')

scaling = list(100/0.8, exp, 1, 100, 1, 1, 1, 100, 100, NaN, NaN, 100)


pr_limits      = c(100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)
drought_limits = seq(0, 1, 0.1)
limits = list(c(10, 20, 30, 40, 50, 60, 70, 80, 90),
              pr_limits,
              c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30),
              c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50),
              drought_limits,
              c(12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36),
              c(0.1, 1, 10, 100, 1000),
              c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1),
              seq(5, 60, 5),
              NaN,
              NaN,
              seq(5, 60, 5))

tas_colour = c('#5e4fa2', '#66c2a5', '#e6f598', '#fee08b', '#f46d43', '#9e0142')
agr_colour = c('#fff7f3', '#fcc5c0', '#f768a1', '#ae017e', '#49006a', '#120017')
prc_colour = c('#ffffd9', '#c7e9b4', '#41b6c4', '#225ea8', '#081d58', '#020d3a')
drt_colour = c('#ffffe5', '#fee391', '#fe9929', '#cc4c02', '#662506', '#160601')

cols = list(c("white" , "#AAAA00", "#003300"),
            prc_colour,
            tas_colour,
            c('#ffffcc', '#fed976', '#fd8d3c', '#e31a1c', '#800026', '#200008'),
            drt_colour,
            tas_colour,
            c('#f7fcfd', '#bfb3e6', '#8c96c6', '#88419d', '#4d004b', '#130012'),
            c('white', 'black'),
            agr_colour,
            'white',
            'white',
            agr_colour)

box = list(NULL, c('#0000AA' = 1),
          c('#FFFF00'  = 1),
          c('#AA0000' = 1), c('#AA0000' = 1), c('#AA0000' = 1),       
          c('red' = 1,    'black'   = 2),
          c('black'   = 1), c('black'   = 1), NULL, NULL, c('black' = 1))                


png('figs/inputs_map.png', height = 8, width = 16, units = 'in', res = 300)
    par(mfrow = c(4, 3), mar = c(0, 0, 1, 0), oma = c(0, 0, 1, 0))
    plot_inputs_figure(add_legend = TRUE)
dev.off.gitWatermark()
###############
## figure S2 ##
###############

plot_vars =  paste0(rep(c("MAP", drought_vars), each = 4), '_', pr_datasets)

names(plot_vars) = paste(LETTERS[1:16], '', rep(c("MAP", drought_vars), each = 4), pr_datasets)


units   = c(rep('mm/yr', 4), rep('', 16))

scaling = c(rep(c(exp), 4), rep(1, 16))

limits = c(rep(list(pr_limits), 4), rep(list(drought_limits), 16))

cols = c(rep(list(prc_colour), 4), rep(list(drt_colour), 16))

box = rep(list(NULL), 16)

png('figs/inputs_pr_dr_map.png', height = 7, width = 16, units = 'in', res = 300)
    layout(rbind(1:4, 21, 5:8, 9:12, 13:16, 17:20, 22), heights = c(1, 0.3, 1, 1, 1, 1, 0.3))
    par( mar = c(0, 0, 0, 0), oma = c(0, 0, 1, 0))
    plot_inputs_figure() 
    
    plot.new()
    addStandardLegend(dats[[plot_vars[1]]], pr_limits, prc_colour) 

    plot.new() 
    addStandardLegend(dats[[plot_vars[5]]], drought_limits, drt_colour)
dev.off.gitWatermark()
