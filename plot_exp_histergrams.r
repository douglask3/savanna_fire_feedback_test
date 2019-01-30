########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

breaks_default = seq(0.1, 0.8, 0.01)
axis_at = seq(0.1, 0.8, 0.1)

anomilies = TRUE
transform = TRUE
########################################
## load and analyes  			      ##
########################################

dat = dats[[1]][['TreeCover']]
out = makeOrLoadEnsembles()	
out = selectOutput(out)

addAnololie <- function(r_exp, r_obs, r_cnt, maxV = 0.8) {
	
	trans <- function(r) { 
		r = r / maxV
		r[r > 1] = maxV
		
		return(r)
	}
	
	r_exp = trans(r_exp)
	r_obs = trans(r_obs)
	r_cnt = trans(r_cnt)
	
	if (transform) r_obs = logit(r_obs)
	
	r_anm = r_obs * r_exp / r_cnt
	
	if (transform) r_anm = logistic(r_obs * r_exp / r_cnt, 0, 1)
	return(r_anm * maxV)
}

if (anomilies) aout = lapply(out,addAnololie, dat, out[[1]]) else aout = out

dout = lapply(out, function(i) i -out[[1]])
plot_hist <- function(r, title, axis = TRUE, yline = NULL, poly = TRUE, maxY = NULL,
				      breaks = breaks_default, hrange = range(breaks), log = '') {
	print(maxY)
	if (log == 'y') yrange = c(0.001, 1.0) else yrange = c(0, 1.2)
	plot(hrange, yrange, axes = FALSE, type = 'n', xlab = '', ylab = '', log = log)
	if (axis) axis(1, at = axis_at, labels = axis_at * 100)
	mtext(title, line = -3)
	plotPoly <- function(v, alpha = 0.9, poly = TRUE) {
		hr = hist(v, breaks = breaks, plot = FALSE)
		x = hr$mids
		y = hr$density
		if (is.null(maxY)) maxY = max(y)
		
		y = y/maxY
		if (log == 'y') y[y < 0.001] = 0.001
		if (poly) {
			polygon(c(x, rev(x)), c(y, rep(0, length(y))),
					border = NA, col =  make.transparent('black', alpha))
		} else {
			lines(x, y, col = 'red', lwd = 2.3)
			lines(x, y, col = '#BB0000', lwd = 2)
			lines(x, y, col = '#FF9999', lwd = 2.1, lty = 2)
		}
		return(maxY)
	}
	vr = layer.apply(r, function(i) i[i > hrange[1] & i < hrange[2]])
	maxY_out = lapply(vr, plotPoly, poly = poly)
	if (!is.null(yline))  plotPoly(yline, poly = FALSE)
	return(list(vr, maxY_out))
}

out = plot_hist(dat, 'obs', poly = FALSE)
vdat = out[[1]][[1]]; maxY = out[[2]][[1]]

graphics.off()
png(paste('figs/killer_histergrams', transform, '.png', sep = '-'), height = 9, width = 7, unit = 'in', res = 200)

	par(mfrow = c(4,2), mar = c(0.45, 0.5, 0.1, 0.5), oma = c(2, 0.5, 0.4, 0.5))
	mapply(plot_hist, aout[5:12], names(dats)[5:12], axis = c(rep(F, 6), T, T), 
		   MoreArgs = list(vdat, maxY = maxY))
	   
dev.off.gitWatermark()

png(paste('figs/shift_histergrams', transform, '.png', sep = '-'), height = 9, width = 7, unit = 'in', res = 200)

	par(mfrow = c(4,2), mar = c(0.45, 0.5, 0.1, 0.5), oma = c(2, 0.5, 0.4, 0.5))
	mapply(plot_hist, dout[5:12], names(dats)[5:12], axis = c(rep(F, 6), T, T), 
		   MoreArgs = list(maxY = NULL, breaks = seq(0.01, 0.5, 0.01), log = ''))
	   
dev.off.gitWatermark()