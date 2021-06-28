########################################
## cfg          		      ##
########################################
source("cfg.r")
graphics.off()

limits4 = c(0.2, 0.4, 0.6, 0.8)
limits4 = c(0.33, 0.67)
cols4 = c("FF", "BB","88", "44", "00")
cols4 = c("FF", "88", "00")

col4Labs = list(c('S', 's', ''), c('E', 'e', ''),  c('P', 'p', ''), c('T', 't', ''))

limits = list(c(1, 2, 5, 10, 25, 50, 75, 90, 95, 98, 99),
              c(1, 2, 5, 10, 20, 30, 40, 50, 60),
              c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90))
limits2 = c(0.2, 0.5, 1, 2,5, 10, 20)
cols = c("white", "#AAAA00", "#003300")

dlimits = c(-40, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 40)
dcols = c("#330000", "#DD0033", "#FF33FF", "white", "#FFFF00", "#00FF00", "#003300")
items = c(2:3, 5:6)

summaryFile = "model_summary-nEns-11.nc"
allPostDir = "data/sampled_posterior/attempt6//control//"
controls = c(Stress = "mortality", Exclusion = "exclude", MAP = "map", Energy = "energy")
limTypes = c("Standard limitation" = "standard",
             "Potential limitation" = "potential", "Sensitivity" = "sensitivity")
normalies = c(T, T, T, F)

########################################
## load and analyes  			      ##
########################################

plotMap <- function(limType, limits, title = '',
                    fname  = '', ids = c(3, 7), header = c('10%', '90%')) {
    lim_varnames = paste0(limType, '_', controls)
    lims = lapply(lim_varnames, function(v) brick(paste0(allPostDir, summaryFile), varname = v))
    
    fname = paste('figs/limPlot', fname, '-', limType, '.png', sep = '')
    png(fname, height = 4.67, width = 12, res = 300, unit = 'in')
	print(fname)
        if (limType == "standard") {
            heights = c(1,1,1,0.5, 1)
            r1 = c(10, 10, 11); r2 = 12
        } else {
            heights = c(1,1,1,1,0.5)
            r1 = 10:12; r2 = c(13, 13, 14)
        }
        
	layout(rbind(1:3, 4:6, 7:9, r1, r2), heights = heights)
	par(mar = c(0.5, 0, 0.5, 0), oma = c(0,0,2.3, 0))	
	
	plotFun <- function(lim, title, addTotTitle, normalise) {
            limx = lapply(c(3,7), function(x) lim[[x]]*100)
            limx = lapply(limx, function(r) {r[r>9E9] = NaN; r})
            limx = lapply(limx, function(r) {r[lims[[4]][[1]]>9E9] = NaN; r})
            
            if (normalise) {
                mx = max(sapply(limx, max.raster, na.rm = TRUE))/100
                limp = lapply(limx, '/', mx)
            } else limp = limx
            limp = c(limp, limp[[2]] - limp[[1]])
	    FUN <- function(r, limitsi = limits)
                plotStandardMap(r, cols = cols, limits = limitsi, interior = FALSE)
            FUN(limp[[1]], limits)
            #if (limType == "sensitivity") browser()
            mtext(side = 2, title, line = -2)
            if (addTotTitle) mtext(side = 3, header[1], line = 0)
	    mtext(side = 2, adj = 0.5, title)
            FUN(limp[[2]], limits)
            
            if (addTotTitle) mtext(side = 3, header[2], line = 0)
            FUN(limp[[3]], limits2)  
            if (addTotTitle) mtext(side = 3, 'Range%', line = 0)            
            return(limx)          
	}	
	if (limType == "standard") index = 1:3 else index = 1:4
	limsx = mapply(plotFun, lims[index], names(controls)[index],
                      c(T, rep(F, length(lims)-1))[index],
                       normalies[index], SIMPLIFY = FALSE)    
        title(title, outer = TRUE, line = 1)    
        addStandardLegend(lims[[1]][[1]], limits , cols, units = '', add = FALSE, 
                          plot_loc = c(0.2, 0.8, 0.55, 0.7), maxLab = 100, srt = 45)      
        addStandardLegend(lims[[1]][[1]], limits2 , cols, units = '', add = FALSE, 
                          plot_loc = c(0.1, 0.9, 0.55, 0.7), extend_max = TRUE, srt = 45)
    dev.off()

    
    return(limsx)
}	
plotAllTypes <- function(...) 
    mapply(plotMap, limTypes, limits, names(limTypes),
            MoreArgs = list(...), SIMPLIFY = FALSE)

plotAllTypes(fname = '10-90', ids = c(3, 7), header = c('10%', '90%'))
limsxs = plotAllTypes(fname = '25-75', ids = c(4, 6), header = c('25%', '75%'))
test = is.na(limsxs[[1]][[3]][[2]]) & !is.na(limsxs[[1]][[1]][[1]])
limsxs[[1]][[3]][[2]][test] = 0.0

plotFUN4ways <- function(rs, fname, title,
                         normalise = FALSE, revcols = normalise, limits4Scale = 1) {
    
    limits4 = 1-limits4Scale*(1-limits4)
    	
    plotRanges <- function(i, topTitle) { 
        plotRange <- function(id, sideTitle) {
            #is = rep(3-i, 4)
            if (revcols) {i1 = 3-i; i2 = i} else {i1 = i; i2 = 3-i}
            is = rep(i1, 4)
            is[id] = i2
            print(is)
            r = mapply(function(r, j) r[[j]], rs, is)
            pout = lapply(r, values)
            pout = lapply(pout, function(i) (100-i)/100)
	    xy = xyFromCell(r[[1]], 1:length(pout[[1]]))
            wow = pout[[4]]
            if (revcols) {
                cols4 = rev(cols4)
                #pout[[4]] = 1 - pout[[4]]
                pout[[4]] = 1-limits4Scale*(pout[[4]])
            }
            
            plot(c(-120, 160), c(-30, 30), axes = FALSE, xlab = '', ylab = '', type = 'n')      
            yay = plot_4way(xy[,1], xy[,2], pout[[3]], pout[[1]], pout[[2]],pout[[4]],
	             x_range = c(-180, 180), y_range = c(-30, 30),
	             cols = c(cols4), limits = limits4, 
	             coast.lwd=par("lwd"),ePatternRes = 30, ePatternThick = 0.5,
	             add_legend=FALSE, smooth_image=FALSE,
                      smooth_factor=5, normalise = normalise, add = TRUE)
            if (i == 1) mtext(side = 2, sideTitle, line = -1)  
                  
        }
	    #par(mar = c(3, 10, 0, 8))
        plotRange(1, names(controls)[1])
        mtext(side = 3, topTitle)
        mapply(plotRange, 2:3, names(controls)[2:3])        
    }
    png(paste0("figs/lim4way-", fname, ".png"), height = 0.3+4*4/4, width = 0.1 + 12*3/4, res = 300, unit = 'in')
        layout(rbind(cbind(1:3, 4:6), 7))
        par(mar = c(0.5, 0, 0.5, 0), oma = c(0, 1, 1, 0))
        mapply(plotRanges, 1:2, c('10%', '90%'))
        par(mar = c(0.5, 5, 0.25, 5))
        legend4way(limits4, cols4, col4Labs, lbRt = 25)
        title(title, outer = TRUE)
    dev.off()
}

mapply(plotFUN4ways, limsxs, limTypes, names(limTypes), c(F, F, F), c(F, T, T), c(1, 1/3, 1/2))

