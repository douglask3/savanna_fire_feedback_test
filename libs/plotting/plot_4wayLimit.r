plot_4wayLimit <- function(pmod, lab = '', remove1 = TRUE,
						   cols = rev(c("FF","CC","99","55","11")), 
							...) {
	if (remove1) pmod = pmod[-1] # remove first element of simulated fire
	xy = xyFromCell(pmod[[1]], 1:length(pmod[[1]]))
	pmod = lapply(pmod, values)

	plot_4way(xy[,1], xy[,2], pmod[[3]], pmod[[1]], pmod[[2]], pmod[[4]],
			  x_range = c(-180, 180), y_range = c(-60, 90),
			  cols=cols,
			  coast.lwd=par("lwd"),
			  add_legend=FALSE, smooth_image=FALSE, ...)
			  
	
	mtext(lab, line = -1, adj = 0.05)
}