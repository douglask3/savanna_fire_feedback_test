source("cfg.r")
graphics.off()
options(scipen = 999)

cols = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
limits = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40)

index_file =  "index-nEns-1001-from-101.csv"
PostDir = "data/sampled_posterior/attempt15/"

conID  = "control"
expIDs = c("Burnt\narea" = "noFire", "Heat\nstress" = "noTasMort", "Wind" = "noWind",
            "Rainfall\ndistribution" = "noDrought", "Population\ndensity" = "noPop",
            "Urban\narea" = "noUrban", "Cropland\narea" = "noCrop", "Pasture\narea" = "noPas")

samples = list.files(paste0(PostDir, '/', conID), recursive = TRUE)
samples = samples[grepl('sample', samples)]

index = read.csv(paste0(PostDir, '/', conID, '/', index_file))
samples = samples[index[,2]]

biomes = raster::resample(raster('data/biomAssigned.nc'),
                          raster('data/driving_Data/TROPICS/TreeCover.nc'))

nexp = length(expIDs)
beats = matrix(0, nrow = nexp, ncol = nexp)
openCompareSample <- function(sample, biomeN = NULL, mask = NULL) {
    print(sample)
    tfile = paste0(paste0("temp/beats-", paste0(strsplit(sample, '/')[[1]],  collapse = '-')),
                   biomeN, length(samples), '.Rd')
    
    if (file.exists(tfile)) {
        load(tfile)     
        return(beats)
    }
    print(biomeN)
    print(which(samples==sample))
    openDat <- function(id) {
        dat = raster.NaN(paste0(PostDir, id, '/', sample), varname = "tree_cover_mean")
        if (!is.null(mask)) return(sum(dat[mask], na.rm = TRUE))
        else return(sum.raster(dat, na.rm = TRUE))
    }
    dats = sapply(expIDs, openDat)
    
    for (i in 1:nexp) beats[i,] = beats[i,] + (dats[i] > dats)

    save(beats, file = tfile)
    return(beats)
}


forBiome <- function(biomeN = NULL) {
    if (!is.null(biomeN)) mask = biomes == biomeN & !is.na(biomes) else mask = NULL
    beatss = lapply(samples, openCompareSample, biomeN, mask)
    for (i in beatss) beats = beats + i
    beats = 100*beats/length(beatss)
    rownames(beats) = expIDs
    colnames(beats) = expIDs

    write.csv(beats, file = paste0("outputs/beatsTable", biomeN, ".csv"))
}

#forBiome(4)
#forBiome()

#forBiome(2:8)#
cols = c('#f7f4f9','#e7e1ef','#d4b9da','#c994c7','#df65b0','#e7298a',
         '#ce1256','#980043','#67001f')

limits = c(1, 2, 5, 10, 25, 50, 75, 90, 95, 98, 99)

colsVs = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')

colsVs = make_col_vector(colsVs, limits = limits)

whereBeat <- function(ids, newplot = TRUE) {
    forSample <- function(sample) {
        tfile = paste0(c("temp/whichBeat", ids, strsplit(sample, '/')[[1]], '.nc'), 
                       collapse = '-')
        
        if (file.exists(tfile)) return(raster(tfile))
        print(ids)
        print(which(samples==sample))
        forID <- function(id)
            dat = raster.NaN(paste0(PostDir, id, '/', sample), varname = "tree_cover_mean")
        
        dat = which.max(layer.apply(ids, forID))
        dat = writeRaster(dat, filename = tfile, overwrite = TRUE)
        return(dat)
    }
    dats = layer.apply(samples[1:25], forSample)
    dats = 100*layer.apply(1:length(ids), function(id) sum(dats == id))/nlayers(dats)
    
    titles = sapply(ids, function(id) names(expIDs[which(id == expIDs)]))
    
    if (newplot) {
        lmat = matrix(1:length(ids), ncol = ceiling(length(ids)/4))
        lmat = rbind(lmat+1, 1)
        png(paste0(c("figs/whichBiggest", ids, '.png'), collapse = '-'),
            res = 300, height = 1.5*(nrow(lmat) - 0.7), width = ncol(lmat)*5, units = 'in')
            layout(lmat, heights = c(rep(1, nrow(lmat)-1), 0.3))
            par(mar = c(0.75, 0, 0.75, 0), oma = c(1, 0, 1, 0))
            addStandardLegend(dats[[1]], limits, cols, '%', add = FALSE, srt = 0, maxLab = 100,
                          plot_loc = c(0.1, 0.9, 0.55, 0.8))
    
            mapply(plotStandardMap, layers2list(dats), txt = titles, 
                   MoreArgs = list(cols = cols, limits = limits))
        dev.off()
    } else {     
        firstLetters <- function(txt) 
            paste(substr(strsplit(txt, '\n')[[1]], 1,1), collapse = '')   
        plotStandardMap(dats[[1]], cols = colsVs, limits = limits)        
        legend(x = -40, y = -10, pch = 15, col = colsVs[1],
               legend = firstLetters(titles[2]), bty = 'n', horiz = TRUE, cex = 0.67) 
        legend(x = 55, y = -10, pch = 15, col = tail(colsVs, 1),
               legend = firstLetters(titles[1]), bty = 'n', horiz = TRUE, cex = 0.67)
        
    }
    return(dats)
}

png("figs/likiAbeatB.png",width = 12, height = 3.1, res = 300, units = 'in')
lmat = matrix(0, ncol = (nexp), nrow = (nexp))
n = 1
for (i in 1:(nexp)) for (j in 1:(nexp)) {
    if (i >= j) {
        n = n + 1
        lmat[j, i] = n
    } else if (j >(nexp-2)) lmat[j, i] = 1
}
layout(lmat)
par(mar = rep(0, 4), oma = c(1, 0, 0, 0))
addStandardLegend(eg[[1]], limits, colsVs, '%', add = FALSE, srt = 0, maxLab = 100,
                          plot_loc = c(0.1, 0.9, 0.55, 0.6))
mtext(side = 3, 'Liklihood A > B', line = -2)
whereBeatPair <- function(j,i ) {
    print(i)
    print(j)
    print("====")
    ids = c(expIDs[i], expIDs[j])
    titles = sapply(ids, function(id) names(expIDs[which(id == expIDs)]))
    print(titles)
    print("===")
    if (i < j) return()
    if (i == j) {
        plot.new()
        mtext(titles[1], side = 3, line = -1, adj = 0.9, padj = 1)
    
        return()
    }
    whereBeat(ids, FALSE)
    #Sys.sleep(0.3)
    #text(0.5, 0.5, paste0(i, ',', j))
}
out = lapply(1:nexp, function(i) lapply(1:nexp, whereBeatPair, i))
dev.off()

#whereBeat(expIDs[1:4])
#whereBeat(expIDs)


