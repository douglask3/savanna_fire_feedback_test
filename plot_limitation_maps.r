########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

limits = c(1, 10, 20, 30, 40, 50, 60, 70)
cols = c("white", "#AAAA00", "#003300")


limits_stn = c(0.25, 0.5, 0.75)#c(0.1, 0.25, 0.75, 0.9)
limits_pot = limits_stn#c(0.2, 0.4, 0.6, 0.8)
limits_sen = limits_stn#c(0.2, 0.4, 0.6, 0.8)
cols = c("FF", "BB","88", "44", "00")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")
items = c(2, 6, 5, 4)

########################################
## load and analyes  			      ##
########################################

plot4pr_droughts <- function(fire_dataset, pr_dataset, drought_var) {
    out = makeOrLoadEnsembles(fire_dataset = fire_dataset, pr_dataset = pr_dataset,
                              drought_var = drought_var)
    lim = lapply(items, function(i) selectOutput(out, i)[[1]])
    sen = lapply(items, function(i)  tail(selectOutput(out, i), 1)[[1]])
    
    xy = xyFromCell(lim[[1]], 1:length(lim[[1]]))
    mlim = lim
    
    tot = lim[[1]] * lim[[2]] * lim[[3]] * lim[[4]]
    
    pot <- function(i) {
        limi = lim[-i]
        return((limi[[1]] * limi[[2]] * limi[[3]] - tot))        
    }    
    nlim = lapply(1:4, pot)
    #nlim[[2]] = nlim[[2]] * 10
    tot = nlim[[1]] + nlim[[2]] + nlim[[3]] + nlim[[4]]
    nlim = lapply(nlim, function(i) i/tot)
    
    mlim = lapply(lim, function(i) 1-i)
    #sen[[2]] = sen[[2]] * 10
    #mlim[[3]] = mlim[[3]] * 10
    #nlim = mlim[[1]] + mlim[[2]] + mlim[[3]] + mlim[[4]]
    #nlim = lapply(mlim, '/', nlim)

    plotMap <- function(x, limits, fname = '', normalise = FALSE,
                        ePatternRes = 50, ePatternThick = 0.4) {
	x = lapply(x, function(i) max.raster(i, na.rm = TRUE) * 
                               (i-min.raster(i, na.rm = TRUE))/
                               diff(range.raster(i, na.rm = TRUE)))
	
	#2.5:4.5
	pout = lapply(x, values)
	
	if (normalise) cols = rev(cols)
	# if rev: c,m,y else: c,m,y
	
        print(limits)
	plot_4way(xy[,1], xy[,2], pout[[1]], pout[[3]], pout[[2]], pout[[4]],
			  x_range = c(-120, 160), y_range = c(-30, 30),
			  cols = c(cols), limits = limits, 
			  coast.lwd=par("lwd"),
                          ePatternRes = ePatternRes, ePatternThick = ePatternThick,
			  add_legend=FALSE, smooth_image=FALSE,smooth_factor=5, 
                          normalise = normalise)
        mask = raster('data/seamask.nc')
        plot_raster_from_raster(mask, x_range = c(-120, 160), y_range = c(-30, 30),
                                add = TRUE, col = c("white", "transparent"), limits = c(0.5),   
                                quick = TRUE, interior = FALSE, coast.lwd = NULL,
                                add_legend = FALSE)
	
    }	

    plotMaps <- function(x, fname, limits, ...) {
	plotStuff <- function(ID1, FUN1, FUN2, FUN3 = NULL, txt = '') {
            x0 = x
	    x[[ID1]] = FUN1(x[[ID1]])
	    x[-ID1] = lapply(x[-ID1], FUN2)
            if (!is.null(FUN3)) x = mapply(function(x, x0) x - FUN3(x0), x, x0)
            #browser()
	    plotMap(x, limits = limits, ...)
            mtext(txt)  
	}
        if (!is.null(fname)) {
            fname = paste('figs/limPlot', fname, '.png', sep = '')
	    png(fname, height = 2.4, width = 4.75, res = 300, unit = 'in')
	    print(fname)
	   # layout(rbind(t(matrix(1:6, ncol = 3)), 7), heights = c(1,1,1,0.5))
	    par(mfrow = c(3, 2), mar = c(1, 0, 1, 0), oma = c(0,0,0.5, 0))
        }
            
	    plotStuff(1, mean, mean)
        if (is.null(fname)) return()  
            mtext(paste(LETTERS[1], '', 'Mean'))  
            add_raster_4way_legend(cols = cols ,limits = limits,
				   labs = c('<- Moisture', 'Fuel ->', 
                                            'Igntions ->', 'Land Use'))
            
	    mapply(plotStuff, 1:4, txt =  paste(LETTERS[2:5], '', 'Max', 
                                                 c('MAP', 'MAT', 'stress', 'exclusion')),
                   MoreArgs = list( function(i) max(i),  function(i) min(i)))
	    #mapply(plotStuff, 1:4, paste('min', 1:4), MoreArgs = list( function(i) min(i),  function(i) max(i)))

        dev.off()#.gitWatermark()
    }
    
    fname = paste0(pr_dataset, '-', drought_var, '-')
    #plotMaps(mlim, paste0(fname, 'standard'), limits_stn)
    plotMaps(nlim, paste0(fname, 'potential'), limits_pot)
    plotMaps(sen, paste0(fname, 'sensitivity'), limits_sen)
    
    fname = paste0('figs/lim_pop_sen_maps', fname, '.png')

    png(fname, width = 4.75, height = 2.7*2, units = 'in', res = 300) 
        layout(rbind(1, 2, 3, 4))
        par(mar = rep(0, 4), oma = c(0, 0, 1, 0))
        plotMaps(mlim, NULL, limits_stn, ePatternRes = 40, ePatternThick = 0.6)
        mtext(paste(LETTERS[1], '', 'Standard limitation'), line = -0.75)
        plotMaps(nlim, NULL, limits_pot, FALSE, ePatternRes = 40, ePatternThick = 0.6)
        mtext(paste(LETTERS[1], '', 'Potential limitation'), line = -0.75)
        plotMaps(sen, NULL, limits_sen, ePatternRes = 40, ePatternThick = 0.6)
        mtext(paste(LETTERS[2], '', 'Sensitivity'), line = -0.75)
    dev.off()
    browser()
}

plot4pr_droughts("GFED_four_s", 'MSWEP', 'MADD')
#runAll_pr_droughts(plot4pr_droughts)

