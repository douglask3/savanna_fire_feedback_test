addStandardLegend <- function(x, limits, cols, units = '%', plot_loc = c(0.38, 0.80, 0.02, 0.05), ylabposScling=2.7, ...)  {
	add_raster_legend2(cols, limits, dat = x, srt = 90,
			   transpose = FALSE, plot_loc = plot_loc, ylabposScling=ylabposScling, oneSideLabels = TRUE, xpd = NA, ...)
	mtext(units, side = 3, lines = -2)
}