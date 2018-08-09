addStandardLegend <- function(x, limits, cols, units = '%',...)  {
	add_raster_legend2(cols, limits, dat = x, srt = 0,
			   transpose = FALSE, plot_loc = c(0.25, 0.75, 0.5, 0.8), ylabposScling=1.5, oneSideLabels = NA, ...)
	mtext(units, side = 3, lines = -1)
}