addStandardLegend <- function(x, limits, cols, units = '%', plot_loc = c(0.32, 0.67, 0.7, 0.9), ylabposScling=1, ...)  {
	add_raster_legend2(cols, limits, dat = x, srt = 0,
			   transpose = FALSE, plot_loc = plot_loc, ylabposScling=ylabposScling, oneSideLabels = TRUE, xpd = NA, adj = 1.0, units = units, ...)
	#mtext(units, side = 3, line = 0)
}
