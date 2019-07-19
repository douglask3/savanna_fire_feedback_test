######################
## cfg				##
######################
source("cfg.r")
graphics.off()

dat = loadInputData()
#paramFile = paste0(paramFile, '_', pr_datasets[1], '_', drought_vars[1], '.csv')
#params = read.csv(paramFile, stringsAsFactors=FALSE)

temp_file  = 'temp/plot_all_mort_dat2.Rd'
grab_cache = TRUE

controls   = c(2, 8, 6, 7, 5, 9, 10, 11, 12, 13)
varnames   = c("MAP_MSWEP", "MADD_MSWEP", "MADD_MSWEP", "MADD_MSWEP", "BurntArea", "MTWM", "PopDen", "urban", "crop", "pas")
yaxis      =  c(T, T, F, F, T, F, F, T, F, T)
xlog       = c(F, F, F, F, F, F, T, F, F, F)
titles     = c("MAP (mm/yr)", "Rainfall Distribution", "Rainfall Dist. on MAP", "Rainfall Dist. on Stress", "Burnt area (%)", "Temperature (~DEG~C)", "Population /k~m2~", "Urban area (%)",
             "Cropland area (%)", "Pasture area (%)")
scaling    = c(1, 1, 1, 1, 100, 1, 1, 100, 100, 100)
xmax       = c(4000, 1, 1, 1, 100, NaN, NaN, 100, 100, 100)


standardLine <- function(scaling, ...) lines(c(-9E9, 9E9) * scaling, c(9E9, -9E9) * 100, ...)
zeroLine <- function(scaling, ...) lines(c(-9E9, 9E9), c(0, 0), ...)

linesFUNs = list(function(scaling, ...) lines(c(4000, 4000) * scaling, c(-9E9, 9E9), ...),
                 zeroLine,  zeroLine, zeroLine,
                 standardLine, zeroLine, NULL, standardLine, standardLine, standardLine)

add_jules  = TRUE

JULES_dir  = "../jules_outputs/"

JULES_control     =  "u-bk543"
JULES_experiments =  list("u-bk757", "u-bk716",  NULL, NULL,"u-bk809", "u-bk807", NULL, NULL, "u-bk811", "u-bk812")

yrs   = c(2002, 2008, 2014)

alphaF = 0.01

Dcols = unlist(lapply(1:8,function(i) rep(rev(blues9)[i], round((9-i)^2))))



######################
## open				##
######################
if (file.exists(temp_file) && grab_cache) { load(temp_file)
} else {
    out = makeOrLoadEnsembles()	
    out = selectOutput(out)
    nFun <- function(en, i) 
            (i[[en]] - out[[1]][[en]])/max(addLayer(i[[en]], out[[1]][[en]]))

    dout = lapply(out, function(i)
                  layer.apply(1:nlayers(i), nFun, i))
    #dout = lapply(out, function(i) (i-out[[1]])/max(addLayer(i, out[[1]])))
    save(out, dout, file = temp_file)
}

if (add_jules) {
    index = !sapply(JULES_experiments, is.null)
    c(Jules_TC_control, Jules_fire, Jules_TC_exp, Jules_dout, Jules_PFTs) :=
        openJulesExperimentSet('for_mort_scatter', JULES_control, JULES_experiments[index], 
                               grab_cache = grab_cache, yrs = yrs) 

    impact <- function(exp) (exp - Jules_TC_control)/max(addLayer(exp,Jules_TC_control))
    jules = layer.apply(Jules_TC_exp, impact)
    JULES_experiments[index ] = layers2list(jules)
} else {
    JULES_experiments = rep(list(NULL), length(controls))
}
######################
## plot				##
######################
do_the_plot <- function(yaxis = FALSE, addLegend = FALSE, title = '', let, control = 5, varname = "BurntArea", jules = NULL, p_param = NULL,
                        xlog = TRUE, scaling = 1, xmax = NaN, linesFUN = NULL)  {
    print(varname)
    x = dat[[varname]] * scaling
    #if (grepl("MAP", varname)) x = exp(x)
    if (xlog) {
        
        if (!is.na(xmax)) minx = 0.0001 * 10^(xmax) else minx = 0.0001
        x[x < minx] = minx
        x = log10(x)
    }
    xlim = range.raster(x, na.rm = TRUE)
    if (!is.nan(xmax)) xlim[2] = xmax
    plot(xlim, c(-100, 100), axes = FALSE, xlab = '', ylab = '', type ='n')#, xaxs = 'i', yaxs = 'i'
    if (yaxis) axis(2)   

    if (xlog) {
        at = seq(-5, xlim[2], 2)
        labels = 10^at
        axis(1, at = at, labels = labels)
    } else {
        axis(1)
        if (!is.null(linesFUN)) linesFUN(scaling, lty = 2, lwd =2) 
    }
    mtext.units(title, side = 1, line = 2.5)
    mtext(let, side = 3, line = -2, adj = 0.1)
        
    addPolygon <- function(y, alphaFl = alphaF) {  # dat[['crop']] + dat[['pas']]
        FUN <- function(quantiles, col, alpha = 1 - alphaFl) 
            quantileDesnityPoly(x[], -y[], xlim = xlim, quantiles = quantiles, ymin = -100,
                                between = TRUE, xlog = FALSE, col = make.transparent(col, alpha))

        FUN(c(0.0 , 1.0 ), 'black'    )
        FUN(c(0.1 , 0.9 ), '#330000'  )
        FUN(c(0.25, 0.75), 'red', 1 - 2 * alphaFl)

    }

    scatterDen <- function(y) {
        #cols = unique(unlist(lapply(1:6,function(i) make_col_vector(rev(blues9)[i:(i+1)],
        #              ncols = 8-i))))
        y = y * (-1)
        cols = densCols(x[], y[], colramp = colorRampPalette(rev(Dcols)), 
                        bandwidth = 0.02)
        points(y[]~x[], pch = 20, cex = 1.33, col = make.transparent(cols, 0.95))
    }
    layer.apply(dout[[control]] * 100, addPolygon) #12
    if (!is.null(jules)) {
        jules = raster::resample(jules, dout[[1]])
        scatterDen(jules * 100)
    }
    layer.apply(dout[[control]] * 100, addPolygon, alphaFl = alphaF/2) #12
}



png("figs/mort_exc_impact_per_unit.png", units = 'in',
    height = 4.75 * 4/3 + add_jules * 3, width = 4.75 + add_jules * 1.5, res = 300)
    layout(rbind(c(1, 0, 0), 2:4, 5:7, 8:10))
    par(oma = c(1.0, 4.5, 0.5, 0), mar = c(3.5, 0, 0, 0.5))
    
    mapply(do_the_plot, yaxis, control = controls, varname = varnames, jules = JULES_experiments,
           xlog = xlog, title = titles, let = LETTERS[1:length(controls)],
           scaling = scaling, xmax = xmax, linesFUN = linesFUNs)
    mtext('Proportion change in tree cover(%)', side = 2, line =2.5, outer = TRUE)
dev.off()
