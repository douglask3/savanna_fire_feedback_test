source("cfg.r")
graphics.off()
options(scipen = 999)

cols = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
limits = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40)

index_file =  "index-nEns-101-from-101.csv"
PostDir = "data/sampled_posterior/attempt15/"

conID  = "control"
expIDs = c("Burnt\narea" = "noFire", "Heat\nStress" = "noTasMort","Wind" = "noWind",
            "Rainfall\ndistribution" = "noDrought", "Population\ndensity" = "noPop",
            "Urban\narea" = "noUrban", "Cropland\narea" = "noCrop", "Pasture\narea" = "noPas")
datFiles = paste0('data/driving_Data/TROPICS/',
                  c("BurntArea_", "MTWM.nc", "MaxWind.nc", "", "PopDen.nc", 
                    "urban.nc", "crop.nc", "pas.nc"))


mnDats = list(0.0, 27.5, 0  , 0,    1   ,  0,   0,   0)
mxDats = list(100, 47.5, 13.7, 0.9, 8800, 40, 100, 100)
scales = c(100, 1, 1, 1, 1, 1, 100, 100)
units  = c('%', '~DEG~C',"m~s-1~", "", "people ~m-2~", "% cover", "% cover", "% cover")
logXs  = c(F, F, F, F, T, F, F, F)

obsFile = 'data/driving_Data/TreeCover.nc'

samples = list.files(paste0(PostDir, '/', conID), recursive = TRUE)
samples = samples[grepl('sample', samples)]

index = read.csv(paste0(PostDir, '/', conID, '/', index_file))
samples = samples[index[,2]]

mask = !is.na(raster.NaN(paste0(PostDir, conID, '/',samples[]), varname = "tree_cover_mean"))

loadDat <- function(expID, datFile, bins, scale, sample = NULL) {
    if (expID == "noFire") {
        if (is.null(sample)) return(NULL)
        var = paste(head(strsplit(strsplit(sample, '/')[[1]][1], '_')[[1]], -2), collapse = '_')
        datFile = paste0(datFile, var, '.nc')
    } else if (expID == "noDrought") {
        if (is.null(sample)) return(NULL)
        var = paste(tail(strsplit(strsplit(sample, '/')[[1]][1], '_')[[1]], 2), collapse = '_')
        datFile = paste0(datFile, var, '.nc')
    }
    dat = dat0 = raster.NaN(datFile)*scale
    mask = mask & !is.na(dat)
    if (expID == "noWind") mask = mask & (dat != 0)
    dat = dat[mask]
    
    #if (expID == "noDrought") bins = quantile(dat, bins)
    
    ids = cut_results(dat, bins)

    cnts = rep(0, 102)
    for (id in ids) cnts[id] = cnts[id] + 1
    return(list(dat, mask, ids, cnts))
}


plotEXP <- function(expID, datFile, scale, mnDat, mxDat, logX = FALSE, selfNorm = FALSE, 
                    units = '', title = '', yaxis = FALSE, mnY = 0, mxY = 100, logY = FALSE) {
    print(expID)
    if (logX) exp( seq(log(mnDat), log(mxDat), length.out = 101)) 
    bins = seq(mnDat, mxDat, length.out = 101)
   
    binX = c(bins[1], bins[-1] - diff(bins))
    c(dat, mask, ids, cnts) := loadDat(expID, datFile, bins, scale, NULL) 
    
    logT = ''
    if (logX) logT = paste0(logT, 'x')
    if (logY) logT = paste0(logT, 'y') 
    plot(c(mnDat, mxDat), c(mnY, mxY), type = 'n', xlab = '', ylab = '', yaxt = 'n', log = logT)
    mtext.units(side = 1, line = 2.5, units)
    mtext(side = 3 - 2 * logY, line = -1, adj = 0.1 + 0.8 * logY, padj = 1 - logY*1.5, title)
    grid()
    if (yaxis) axis(2)

    qrts4Sample <- function(sample) {
        tfile = paste0(c("temp/qrts-", expID, '-', mnDat, '-',logX, '-', selfNorm, '-',
                       mxDat, strsplit(sample, '/')[[1]]), collapse = '-')
        if (file.exists(tfile)) { 
            load(tfile)
            #lines(binX, out[2,], col = 'red', lwd = 2)
            return(list(out, cnts))
        }
        if (is.null(dat))
            c(dat, mask, ids, cnts) := loadDat(expID, datFile, bins, scale, sample) 
        print(sample) 
        #sample = samples[5]
       
        exp = raster.NaN(paste0(PostDir, expID, '/',sample), varname = "tree_cover_mean")
        ctr = raster.NaN(paste0(PostDir, conID, '/',sample), varname = "tree_cover_mean")
        
        exp = 100*((exp-ctr)/exp)
        exp = exp[mask]
        if (selfNorm) exp = exp/dat
        exp[exp <0] = 0
        findRange <- function(i) 
            quantile(exp[i == ids], c(0.25, 0.5, 0.75), na.rm = TRUE)

        out = sapply(1:length(bins), findRange)   
        save(out, cnts, file = tfile)
        list(out, cnts)
    }

    qrtsCN = lapply(samples, qrts4Sample)
    
    percentiles = seq(0.1, 0.9, 0.1)
    qrqr <- function(i) {
        qrt = sapply(qrtsCN, function(smp) smp[[1]][i,])
        apply(qrt, 1, quantile, percentiles, na.rm = TRUE)
    }

    qrts = lapply(1:3, qrqr)

    polygonNAsplit <- function(x, y1, y2, ...) {
        index = c(1, which(is.na(y1 + y2)), length(x)+1)
        #browser()
        polyI <- function(i1, i2) {
            if (i2 == (i1+1)) return()
            i = i1:(i2-1)
            polygonDist(x[i], y1[i], y2[i], ...)
        }
        mapply(polyI, head(index, -1), index[-1])
    }

    nq = length(percentiles)
    addQrts <- function(i, id1, id2, col = 'black') 
        polygonNAsplit(binX, qrts[[id1]][i,], qrts[[id2]][nq-i+1,], 
                   col = make.transparent(col, 0.85), border = NA)
    lapply(1:nq, addQrts, 1, 3)
    lapply(1:nq, addQrts, 2, 2, '#880000')

    cnts = sapply(qrtsCN, function(i) i[[2]])
   
    cnts = apply(cnts, 1, sum, na.rm = TRUE)
    if (expID == "noTasMort") cnts[1] = NaN
    if (expID == "noPop") cnts[1:2] = NaN
    #if (expID == "noFire") cnts[1] = 0
    if (expID == "noDrought") cnts[102] = NaN
    cnts = cnts * 100/max(cnts, na.rm = TRUE)
    linesBW <- function(x, y, ...) {
        lines(x, y, col = "white", ...)
        lines(x, y, lty = 3, ...)
    }
    #if (expID == "noDrought") linesBW(c(-9E9, 9E9), c(50, 50))
    #else 
    linesBW(c(binX[1] - diff(binX[1:2]), binX), cnts)
}

if (T) {
png("figs/potential_mortEx_curves.png", height = 7.2, width = 7.2, res = 300, units = 'in')
par(mfrow = c(3, 3), mar = c(3.5, 0.5, 0.5, 0.5), oma = c(2, 2, 0, 0))

mapply(plotEXP, expIDs, datFiles, scales, mnDats, mxDats, logXs, F, units, names(expIDs),
       head(rep(c(T, F, F), 3), -1))

plot(c(0, 1), c(0, 1), type = 'n', axes = FALSE)

pc = percentiles/1.6 + 0.4
for (i in 1:(length(pc)))  {
    polygonDist(c(0.15, 1), rep(pc[i], 2), rep(tail(pc,i)[1], 2), 
                border = NA, col = make.transparent('black', 0.85))
    polygonDist(c(0.55, 1), rep(pc[i], 2), rep(tail(pc,i)[1], 2), 
                border = NA, col = make.transparent('#880000', 0.85))
}
text(x = 0.35, y = pc[1], 'IQR', adj = c(0.5, 1.5))
text(x = 0.8, y = pc[1], 'Median', adj = c(0.5, 1.5))
text(x = 0.15, y = pc, paste(percentiles*100, '%'), ad = 1.3)
dev.off()
#polygonNAsplit(binX, qrts[[1]][2,], qrts[[3]][1,], col = '#666666')
#polygonNAsplit(binX, qrts[[2]][2,], qrts[[2]][1,], col = 'red')
}
expIDs = c("Burnt\narea" = "noFire", "Urban\narea" = "noUrban", "Cropland\narea" = "noCrop", "Pasture\narea" = "noPas")
datFiles = paste0('data/driving_Data/TROPICS/',
                  c("BurntArea_", "urban.nc", "crop.nc", "pas.nc"))


mnDats = list(0, 0,   0,   0)
mxDats = list(100, 40, 100, 100)
scales = c(100, 1, 100, 100)
units  = c('%', "% cover", "% cover", "% cover")
logXs  = F

png("figs/potential_mortEx_curves_norm.png", height = 7.2*2/3, width = 7.2*2/3, 
     res = 300, units = 'in')
par(mfrow = c(2, 2), mar = c(3.5, 0.5, 0.5, 0.5), oma = c(2, 2, 0, 0))

mapply(plotEXP, expIDs, datFiles, scales, mnDats, mxDats, F, T, units, names(expIDs),
       rep(c(T, F), 2), mnY = 0.001, mxY = 10, logY = TRUE)

dev.off()
