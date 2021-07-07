source("cfg.r")
sourceAllLibs("../benchmarkmetrics/benchmarkMetrics/R")
graphics.off()
summaryFile = "model_summary-nEns-101.nc"
PostDir = "data/sampled_posterior/attempt15/control/"
obsFile = 'data/driving_Data/TreeCover.nc'

limitsT = c(1, 2, 5, 10, 20,40, 60, 80)
colsT = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679',
         '#41ab5d','#238443','#006837','#004529')
colsT = c('#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476',
          '#41ab5d','#238b45','#006d2c','#00441b')

varname = "tree_cover_mode"


colsDats = rbind(c("black", "#ff0000"),
                c("#0000ff", "#ffff00"))

pr_dats = c(GPCC = 'GPCC', CRU = 'CRU', MSWEP = 'MSWEP', CMORPH = 'CMORPH')
dr_vars = c(MADD = 'MADD', MADM = 'MADM', MConc = 'MConc', MDDM = 'MDDM')
ba_dats = c(GFED4s = 'GFED_four_s', GFED4 = 'GFED_four', MCD45 = 'MCD_forty_five', 
            MERIS = 'meris', Fire_CCI = 'MODIS')

#########

sim = brick.NaN(paste0(PostDir,summaryFile), varname = varname)
obs = raster(obsFile)#/0.8
obs = raster::resample(obs, sim)
mask = !is.na(obs + sim[[1]])
vsim = apply(sim[mask], 1, function(i) list(i))
vobs = obs[mask]      /0.8

pos1 = pos2 = obs
nl = nlayers(sim)
whereInPost1 <- function(ob, si) {
    out = which(si[[1]]>ob)
    if (length(out) == 1) out = nl+1 else out = out[1]
    out
}

whereInPost2 <- function(ob, si) 
    100*pnorm((logit(si[[1]][[5]])-logit(ob))/1.5, 0, 1)


#pos1[mask] = mapply(whereInPost1, vobs, vsim)
#
#pos2[mask] = mapply(whereInPost2, vobs, vsim)
#pos2[pos2==0 & mask == 0] = 50
lims_pos =  c(1, 5, 10, 20, 40, 60, 80, 90, 95, 99)
cols_pos =  c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')

if (F){
plotMap <- function(..., limits = limitsT, cols = colsT)
    plotStandardMap(..., limits = limits, cols = cols)
png("figs/eval_mapComparison.png", height = 9.67*2/3, width = 7.2*2/3, units = 'in', res = 300) 
    layout(1:6, heights = c(1, 1, 1, 0.4, 1, 0.4))
    par( mar = c(0.67, 0, 1, 0), oma = c(1, 0, 1, 0))
    plotMap(100*obs/0.81)
    mtext(side = 3, "VCF Observations", line = 0)
    plotMap(100*sim[[2]])
    mtext(side = 3, "Framework - 10%", line = 0)
    plotMap(100*sim[[8]])
    mtext(side = 3, "Framework - 90%", line = 0)
    par(mar = c(1.67, 0, 0, 0))
    addStandardLegend(obs, limitsT, colsT, units = '%', srt = 0, add = FALSE,
                      plot_loc = c(0.2, 0.8, 0.57, 0.85), maxLab = 100) 
    par( mar = c(0.67, 0, 1, 0))
    plotMap(pos2, limits = lims_pos, cols = cols_pos) 
    mtext(side = 3, "Obs. position in posterior", line = 0)  
    par(mar = c(1.67, 0, 0, 0))
    addStandardLegend(pos, lims_pos, cols_pos, units = '%', srt = 0, add = FALSE,
                      plot_loc = c(0.2, 0.8, 0.57, 0.85), maxLab = 100)
dev.off()
}


####

dirs = list.dirs(PostDir)

obsT = obs/0.8

ntested = 10
bechmarkDir <- function(dir) {
    
    files = list.files(dir)
    pfile = files[grepl('.csv', files)]
    files = files[grepl('.nc', files)]
    files = files[grepl('sample_no', files)]
    files = sample(files, ntested, replace = FALSE) 
    nm = tail(strsplit(dir, '/')[[1]], 1)
    
    tfile = paste0("temp/MMcomp-", nm, "-", length(files), '.Rd')
    print(tfile)
    if (file.exists(tfile)) load(tfile) 
    else {
        Ps = read.csv(paste0(dir, '/',pfile))[,2]
        benchmarkFile <- function(file) {
            scores = score(NME(obsT, w = raster::area(obs),
                           raster.NaN(paste0(dir, '/', file), varname = varname)))
            
            if (any(scores > 1)) browser()
            scores
        }
        scores = sapply(files, benchmarkFile)
        
        save(scores, Ps, file = tfile)
    }
    return(list(scores, Ps))
}

scores = sapply(dirs[-1], bechmarkDir)

tfile = "temp/nullModel3.Rd"
if (file.exists(tfile)) {
    load(tfile)
} else {
    nulls = null.NME(obs, w= raster::area(obs), n = 1000)
    mdNull = nulls[[1]]
    mnNull = nulls[[2]]
    rrNull = nulls[[3]]
    save(mdNull, mnNull, rrNull, file = tfile) 
}
bins = seq(0, max(rrNull) + 0.002, by = 0.002) 
x = bins[-1] - diff(bins)  /2


rrs = hist(rrNull, breaks = bins, plot = FALSE)$count
rrs = rrs/max(rrs)
    


cols = apply(colsDats, 1, make_col_vector, limits = 1:length(ba_dats))
cols = apply(cols, 1, make_col_vector, limits = 1:length(pr_dats))
colnames(cols) = ba_dats
rownames(cols) = pr_dats   

weightScores <- function(scorei) {      
    
    out = sapply(1:length(scores[1,]),
                        function(j) scores[1,][[j]][scorei,])# * scores[2,][[j]])
    w = sapply(1:length(scores[1,]), function(j) scores[2,][[j]])
    id = out
    for (i in 1:ncol(out)) id[,i] = i
    w = w[1:nrow(out),]
      
    out = as.vector(out); w = as.vector(w)
    smp =  sample(1:length(out), 1000, replace = TRUE, prob = w)    

    out = out[smp] ; w = w[smp]
    id = as.vector(id)[smp]  
    nms = names(scores[1,])
    print(quantile(out, c(0.1, 0.9)))
    base = hist(out, plot = FALSE, breaks = bins)$count
    mx = max(base)
    base = base/mx
  
    newPlt <- function(xlim, ...) {
        
        plot(range(x), c(0, 1), type = 'n', axes = FALSE, xlab = '', ylab = '',
            xlim = xlim)
        polygonDist(x, rrs, col = 'black')       
         
        lines(c(mdNull, mdNull), c(0, 10), lty = 2, lwd = 2)  
        lines(c(mnNull, mnNull), c(0, 10), lty = 4, lwd = 2) 
        if (scorei == 1)  {
            text(x = max(rrNull), y = 0.5, 'Randomly-resampled', srt = 90, adj = c(0.5, 1)) 
            text(x = mdNull, y = 0.5, 'Median', srt = 90, adj = c(0.5, -0.6))       
            text(x = mnNull, y = 0.5, 'Mean', srt = 90, adj = c(0.5, 1.1)) 
        }
    }

    newPlt( c(0.2, max(x)), log = 'x')
    
    axis(1) 
    polygonDist(x, base, col = 'grey')   
    mtext(paste("Step", scorei), side = 3, line = -1)
    xr = x[range(which(base>0))]
    points(xr, c(-0.3, -0.3), pch = 19, xpd = NA)  
    lines(xr, c(-0.3, -0.3), pch = 19, xpd = NA) 
    plotExp <- function(dr, nm) {
        base = rep(0, length(x))
        newPlt(xr)
        grid()
        if (scorei == 1) mtext(side = 2, line = -1, nm) 
        ql = qu = nsamps = cols
           
        for (pr in pr_dats) for (ba in ba_dats) {
            index = which(grepl(pr, nms) & grepl(ba, nms) & grepl(dr, nms))
            
            smples = unlist(lapply(index, function(i) out[id == i]))
            y = base + hist(smples, breaks = bins, plot = FALSE)$count/mx
         
            polygonDist(x, base, y, border = NA, col = cols[pr, ba])
            #if (any(is.na(y))) browser()
            base = y   
            ql[pr, ba] = round(quantile(smples, c(0.1)), 2)            
            qu[pr, ba] = round(quantile(smples, c(0.9)), 2) 
            nsamps[pr, ba] = round(100* length(smples)/length(out), 2)
        }
        index = which(grepl(dr, nms))
        smples = unlist(lapply(index, function(i) out[id == i]))
        list(quantile(smples, c(0.1, 0.9)), ql, qu, nsamps) 
    }
    out = mapply(plotExp, dr_vars, names(dr_vars)) 
    axis(1)  

    out
#bind(unlist(out), unlist(w))
}
png("figs/benchmarkScores.png", height = 10, width = 7.2  , units = 'in', res = 300)
layout(rbind(matrix(1:15, ncol = 3), c(16, 16, 0)))
par(mar = rep(0.5, 4), oma = rep(2, 4))
wscores = lapply(1:3, weightScores)

plot(c(0, 1), c(0, 1), axes = FALSE, xlab = '', ylab = '', type = 'n')
xy = par("usr")

ncol = ncol(cols)
nrow = nrow(cols)
x = diff(xy[1:2]) * seq(0.5, 0.7, length.out = ncol) + xy[1]
y = diff(xy[3:4]) * seq(0.1, 0.55, length.out = nrow) + xy[3]
inds = matrix(1:(ncol * nrow), ncol = ncol)

image(x = x, y = y, z = t(inds), add = TRUE, col = as.vector(cols), xpd = NA)
    text(x = x, y = max(y) + diff(y[1:2])/1.5, names(ba_dats), srt = 45, adj = 0)
    text(x = max(x) + diff(x[1:2])/1.5, y = y, names(pr_dats), srt = 45, adj = 0, xpd = NA)
dev.off()
