######################
## cfg				##
######################
source("cfg.r")
graphics.off()

dat = loadInputData()
paramFile = paste0(paramFile, '_', pr_datasets[1], '_', drought_vars[1], '.csv')
params = read.csv(paramFile, stringsAsFactors=FALSE)

temp_file = 'temp/plot_all_mort_dat.Rd'
grab_cache = TRUE

######################
## open				##
######################	
## Inferened
#if (file.exists(temp_file) & grab_cache) {
#    load(temp_file)
#} else {
    out = makeOrLoadEnsembles()	
    
    out = selectOutput(out)
    dout = lapply(out, function(i) 1-out[[1]]/i)
	
#    save(out, Jules_TC_fire_off, Jules_fire, Jules_TC_fire_on, Jules_dout, dout, Jules_PFTs, file = temp_file)
#}

######################
## plot				##
######################
do_the_plot <- function(yaxis = FALSE, addLegend = FALSE, title = '', let, control = 5, varname = "BurntArea", p_param = NULL,
                        xlog = TRUE, scaling = 1, xmax = NaN)  {
    x = dat[[varname]] * scaling
    if (xlog) {
        x[x < 0.0001] = 0.0001
        x = log10(x)
    }
    xlim = range.raster(x, na.rm = TRUE)
    if (!is.nan(xmax)) xlim[2] = xmax
    plot(xlim, c(0, 100), axes = FALSE, xlab = '', ylab = '', type ='n')#, xaxs = 'i', yaxs = 'i'
    if (yaxis) axis(2)   

    if (xlog) {
        at = seq(-5, xlim[2], 2)
        labels = 10^at
        axis(1, at = at, labels = labels)
    } else {
        axis(1)
        lines(c(-9E9, 9E9) * scaling, c(-9E9, 9E9) * 100, lty = 2, lwd =2)
    }
    mtext.units(title, side = 1, line = 2.5)
    mtext(let, side = 3, line = -2, adj = 0.1)
        
    addPolygon <- function(y) {  # dat[['crop']] + dat[['pas']]
        FUN <- function(quantiles, col, alpha = 0.99) 
            quantileDesnityPoly(x[], y[], xlim = xlim, quantiles = quantiles, 
                                between = TRUE, xlog = FALSE, col = make.transparent(col, alpha))

        FUN(c(0.0 , 1.0 ), 'black'    )
        FUN(c(0.1 , 0.9 ), '#330000'  )
        FUN(c(0.25, 0.75), 'red', 0.98)

    }
    layer.apply(dout[[control]] * 100/ 0.8, addPolygon) #12
}


controls = c(5, 6, 7, 8, 9, 10, 11)
varnames = c("BurntArea", "MADD_MSWEP", "MTWM", "PopDen", "urban", "crop", "pas")
yaxis    =  c(T, F, F, T, F, F, T)
xlog     = c(F, F, F, T, F, F, F)
titles   = c("Burnt area (%)", "Rainfall Distribution", "Temperature (~DEG~C)", "Population /k~m2~", "Urban area (%)",
             "Cropland area (%)", "Pasture area (%)")
scaling = c(100, 1, 1, 1, 100, 100, 100)
xmax    = c(100, 1, NaN, NaN, 100, 100, 100)
png("figs/mort_exc_impact_per_unit.png", units = 'in', height = 4.75, width = 4.75, res = 300)
    par(mfrow = c(3, 3), oma = c(1.0, 4.5, 0.5, 0), mar = c(3.5, 0, 0, 0.5))
    mapply(do_the_plot, yaxis, control = controls, varname = varnames, xlog = xlog, title = titles, let = LETTERS[1:7], scaling = scaling, xmax = xmax)
    mtext('Proportion of tree cover removed (%)', side = 2, line =2.5, outer = TRUE)
dev.off()
