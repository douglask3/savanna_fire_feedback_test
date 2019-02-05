plotStandardMap <- function(x, limits, cols, txt = '', add_legend = FALSE,...) {
	
    plot_raster_from_raster(x, limits = limits, cols = cols,x_range = c(-120, 160),y_range = c(-30, 30),
                            transpose = FALSE, srt = 0,
                            plot_loc = c(0.35,0.83,0.01,0.04),
                            quick = TRUE, add_legend = add_legend, ...)
    mtext(txt,side = 3, line = -1.5)
}

plotStandardMap.sd <- function(x, sc = 1, ...)
	plotStandardMap(mean(x) * sc, 
					e = sd.raster(x), ePatternRes = 35, ePatternThick = 0.4, limits_error = c(0.1, 0.25),...)