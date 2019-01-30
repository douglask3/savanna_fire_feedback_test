quantileDesnityPoly <- function(x, y, xlim = range(x, na.rm = TRUE), nbins = 100, 
								quantiles = c(0.01, 0.05, 0.1, 0.5, 0.9, 0.95, 0.99),
								ymin = 0.000001, xlog = FALSE, col = make.transparent('black', 0.7), between = FALSE) {#seq(0.01, 0.99, 0.01)
	x0 = x; y0 = y
	
	if (xlog) {
		if (xlim[1] < 0.00001) xlim[1] = 0.00001
		bins = 10^(seq(log10(xlim[1]), log10(xlim[2]), length.out = nbins + 1))
	}else
		bins = seq(xlim[1], xlim[2], length.out = nbins + 1)
	
	vx = x[]
	vy = y[]
	findBinRanges <- function(x1, x2) 
		quantile(vy[vx < x2 & vx > x1], quantiles, na.rm = TRUE)
	
	
	y = mapply(findBinRanges, head(bins, -1), bins[-1])
	x = bins[-1] - diff(bins)/2
	mask = !(apply(is.na(y), 2, any) | (apply(y, 2, sum) == 0))
	y[y < ymin] = ymin
	y = y[, mask]
	x = x[mask]
	
	if (between) {
		ploygonQs <- function(y1, y2) 
			polygon(c(x, rev(x)), c(y1, rev(y2)), border = NA, col = col)
		for (i in 1:(floor(nrow(y))/2)) ploygonQs(y[i,], y[nrow(y) - i + 1,])
	} else {
		ploygonQs <- function(qy) 
			polygon(c(x, rev(x)), c(qy, rep(ymin, length(x))), border = NA, col = col)
		apply(y, 1, ploygonQs)
	}
}