plotStandardMap <- function(x, limits, cols, txt = '', add_legend = FALSE, mtext_line = 1,
                           ...) {
	
    plot_raster_from_raster(x, limits = limits, cols = cols,
                            x_range = c(-120, 160), y_range = c(-30, 30),
                            transpose = FALSE, srt = 0,
                            plot_loc = c(0.35,0.83,0.1,0.4),
                            quick = TRUE, add_legend = FALSE, ...)
    polygon(c(-180, 180, 180, -180), c(-90, -90, -31, -31), col = "white", border = "white")
    polygon(c(-180, 180, 180, -180), c(90, 90, 31, 31), col = "white", border = "white")
    mtext(txt, side = 3, line = mtext_line)
    if (add_legend) addStandardLegend(x, limits, cols, 
                                       plot_loc = c(0.1, 0.9, -0.15, -0.05), ...)
}

plotStandardMap.sd <- function(x, sc = 1, ...)
	plotStandardMap(mean(x) * sc, e = sd.raster(x),
                        ePatternRes = 35, ePatternThick = 0.3, 
                        limits_error = c(0.1, 0.25),...)
