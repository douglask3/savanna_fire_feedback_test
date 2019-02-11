source("cfg.r")

dat = loadInputData()

plot_experiment_lines <- function(pr_dataset, drought_var) {
    paramFile = paste0(paramFile, '_', pr_dataset, '_', drought_var, '.csv')
    
    params = read.csv(paramFile, stringsAsFactors=FALSE)
    param <- function(p, line = ensemble_no) params[line, p]

    maxTC = param('max_T') * logistic(0,  param('mort_x0'),  -param('mort_k')) * logistic(0, param('ex_x0'), -param('ex_k'))


    Run <- function(FUN, datVs, params) {

        ins = list(retutnVar = TRUE)
        for (i in 1:length(datVs))  ins[i + 1] = dat[[datVs[i]]]
        
        ensemble <- function(line) {
            for (j in 1:length(params)) ins[j + i + 1] = param(params[j], line)
            do.call(FUN, ins)
        }
        dat = layer.apply(ensemble_no, ensemble)
        return(dat)
    }
    
    y = dat[['TreeCover']]
    pr_var = paste0("MAP_", pr_dataset); dr_var =  paste0(drought_var, '_', pr_dataset)
    x = list(dat[[pr_var]], 
          Run(LimMAT,c("MAT"),c("min_mat", "max_mat")),
          Run(LimMort, c('BurntArea', dr_var,'MTWM','PopDen'), 
                       c('v_drought', 'v_maxTemp', 'v_popDen', 'p_fire', "k_popden", 'p_drought', 
                         'min_maxTemp', 'max_maxTemp', 'p_maxTemp')),
          Run(LimExc, c('urban', 'crop', 'pas'), 
                      c('v_crop', 'v_pas')))
      
    x0 = c('MAP_x0', 'MAT_x0', 'mort_x0', 'ex_x0')
    k  = c('MAP_k' , 'MAT_k' , 'mort_k' , 'ex_k' )
          
    xlim = list(c(0, 3000),
                c(-2, 30),
                c(0, 80),
                c(0, 40))
                
    unlogX = c(T, T, F, F)

    xscale = list(1, param('max_mat') - param('min_mat'), 100, 100)
    xscale = list(1,1, 100, 100)
    xshift = list(0, param('min_mat'), 0, 0)

    units = c('mm/yr', 'C', 'Disturbance area (%)', 'Land use (%)')



    plotControl <- function(x, unlogX, xlim, xscale, xshift, x0, k, ksc, sc, title, units, yaxt) {
        plot(xlim, c(0, 100), type ='n', yaxt = yaxt, xlab = '', ylab = '')
        mtext(side = 1, line = 2, units, cex = 0.8)
        mtext(side = 3, line = -1.5, title, adj = (-ksc/2.3 + 0.5))
        
        y = layer.apply(1:nlayers(x), function(i) y)
        
        if (unlogX) xp = exp(x) else xp = x
        xp = xp * xscale + xshift
        quantileDesnityPoly(xp, y * 100/0.8, xlim, quantiles = c(0.9, 0.95, 0.99, 0.999))
        
        
        xscale = rep(xscale, length.out = length(ensemble_no))
        xshift = rep(xshift, length.out = length(ensemble_no))
        
        x =  seq(xlim[1], xlim[2], length.out = 101)
        xd = mapply(function(i,j) (x-j)/i, xscale, xshift, SIMPLIFY = FALSE)
        if (title == "MAT") xd = lapply(xd, function(i) {i[i<0] = 0; i})
        
        if (unlogX) xd = lapply(xd, logmin)
        
        x0 = params[ensemble_no, x0]; k = params[ensemble_no,k]
            
        y  =  mapply(logistic, xd, x0, k * ksc) #* 0.8# * t(sc)
        y = sweep(y, 2, sc, '*')
        
        if (ksc == -1) y = sweep(y, 2, logistic(0, x0, k*ksc), '/')
        y  = apply(y, 1, quantile, c(0.01, 0.5, 0.99))
        
        #x = x * xscale + xshift
        lines(x, y[1,] * 100, col = 'black', lty = 2, lwd = 1.5)
        lines(x, y[2,] * 100, col = 'black', lty = 1, lwd = 1.5)
        lines(x, y[3,] * 100, col = 'black', lty = 2, lwd = 1.5)
    }
    
    fname = paste0("figs/limitation_line", '_', pr_dataset, '-', drought_var, '.png')
    png(fname, height = 5, width = 5, unit = 'in', res = 300)
        par(mfrow = c(2, 2), mar = c(3.5, 1, 0, 0), oma = c(0, 2, 1, 1)) 
        mapply(plotControl, x, unlogX, xlim, xscale, xshift, x0, k, 
               c(1, 1, -1, -1), list(1, 1, 1, 1),
               plot_title , units, 
               c('s', 'n', 's', 'n'))

        mtext(outer = TRUE, side = 2, line = 1, 'Tree Cover (%)', cex = 0.8)
    dev.off.gitWatermark()
}

runAll_pr_droughts(plot_experiment_lines)	
