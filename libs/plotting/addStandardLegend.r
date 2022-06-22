addStandardLegend <- function(x, limits, cols, units = '%', plot_loc = c(0.38, 0.80, 0.02, 0.05), ylabposScling=1, srt = 90, oneSideLabels = TRUE,...)  {
	add_raster_legend2(cols, limits, dat = x, srt = srt,
			   transpose = FALSE, plot_loc = plot_loc, ylabposScling=ylabposScling, oneSideLabels = oneSideLabels, xpd = NA, adj = 1.0, units = units,...)
	#mtext(units, side = 3, lines = -2)
}
