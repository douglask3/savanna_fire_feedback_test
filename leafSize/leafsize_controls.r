###############
## set up    ##
###############
library(reldist)
library(fields)
library(rstan)
source("libs/return_multiple_from_functions.r")

doGlobal = TRUE
doSites  = FALSE

grab_cache = TRUE
filename ="data/leafsize_datasample.csv"
nchains = 3
nwarmup = 250
niter = 1000
nrefresh = 250

## for for sites with more than this number of samples
nsiteMin = 1


logistic <- function(x, x0 = 0, k = 1) 
  1/(1 + exp(-k*(x - x0)))


###############
## open      ##
###############
dat =  read.csv(filename)
dat = dat[order(dat[,'site.f']),]

sites = dat[['site.f']]
siteIDs = unique(sites)
leafSizes = dat[['LS']]/10000
climMeans = 10^dat[['pmin']]
climMeans = sapply(siteIDs, function(i) climMeans[which(sites==i)[1]])
sites = sapply(sites, function(i) which (siteIDs == i))

selectDat <- function(siteID = NaN) {
    if (!is.na(siteID)) {
        browser()
        test = sites == siteID
        leafSizes = leafSizes[test]
        climMeans  = climMean[test]
    }
    
    leafSize = log(leafSizes)
    climMean = log(climMeans)
    return(list(leafSize, climMean))
}

runBayesian <- function(file, data, init, ...)  {
  fit = stan(file = file, data = data,
             chains = nchains,  warmup = nwarmup, iter = niter, cores = 3, refresh = 250,
             init = rep(list(init), nchains),
             control = list(max_treedepth = 10, adapt_delta = 0.95))
}

runBayesian.prescribedClim <- function(leafSize, climMean) 
  runBayesian("leafSize/prescribedClim.stan", 
              list(nl = length(leafSize), ns =length(climMean), 
                   LS = leafSize, siteIDs = sites, cMu = climMean, #
                   LSmean = mean(leafSize), LSsd = sd(leafSize), climsd = 1),
              list(lsMu = mean(leafSize), lsSigma = sd(leafSize), climSigma = 1))


runBayesian.varyingClim <- function(leafSize, climMean) 
  runBayesian("varyingClim.stan", 
              list(n = length(leafSize), LS = leafSize),
              list(lsMu = mean(leafSize), lsSigma = sd(leafSize),
                   cMu = climMean, climSd = 1))

run4Site <- function(siteID, FUN = runBayesian.prescribedClim, tname = "precribed") {
  ofile = paste('outputs/rstan_contraints_for_site', tname,   
                siteID, nchains, nwarmup, niter, '.csv', sep = '-')
  if (file.exists(ofile) && grab_cache) return(ofile)
  
  c(leafSize, climMean) := selectDat(siteID)
  
  fit = FUN(leafSize, climMean)
  params = data.frame(rstan::extract(fit))
  
  write.csv(params, file = ofile)
  return(ofile)
} 


if (doGlobal) 
    outGlobal = run4Site(NaN)

if (doSites) {
    siteIDs = unique(sites)
    test = sapply(siteIDs, function(siteID) sum(sites == siteID) > nsiteMin)
    siteIDs = siteIDs[test]
    outPrescribed = lapply(siteIDs, run4Site)
}


climMeanMap = log(10^raster("data/leafsie_clim.nc"))


lss_test = c(-20, 20, 0.1)

lss = seq(lss_test[1], lss_test[2], lss_test[3])

ps = read.csv(outGlobal)
mask = !is.na(climMeanMap)
model <- function(eNo = 1, climMean) {
    print(eNo)
    tfileAll = paste0(c("temp/", filename.noPath(outGlobal, TRUE), lss_test, 'summ-ens-2', 
                        eNo, ".nc"), collapse  = '-')
    
    if (file.exists(tfileAll)) return(brick(tfileAll))
    
    c(climSigma, bioMean, bioSigma) := ps[eNo, c('climSigma', 'lsMu', 'lsSigma')]
    
    frequency_ls <- function(ls) {
        
        tfile = paste("temp/", filename.noPath(outGlobal, TRUE), 'ens', eNo,
                      'freqOfLS', ls, '.nc', sep = '-')
        
        if (file.exists(tfile) ) return(raster(tfile))
        out = dnorm(ls, bioMean, bioSigma) * logistic(ls, climMean, -climSigma)
        writeRaster(out, overwrite = TRUE, file = tfile)
    }
    
    tfile = paste(c("temp/", filename.noPath(outGlobal, TRUE), lss_test, 'FreqMaps-ens', 
                  eNo, ".nc"), collapse  = '-')
    
    if (file.exists(tfile)) freq_dist = brick(tfile) 
    else freq_dist = writeRaster(layer.apply(lss, frequency_ls), file = tfile, overwrite = TRUE)
    
    mostFrequent = which.max(freq_dist)
    mostFrequent[] = lss[mostFrequent[]]
    
    wgtedLss = freq_dist[[1]] * lss[1]
    cummSum  = freq_dist
    for (i in 2:nlayers(freq_dist)) {
        #limDiff[[i]] = dnorm(lss[i], bioMean, bioSigma) - climDiff[[i]]
        #if (i > 1) {
        
            wgtedLss = wgtedLss + freq_dist[[i]] * lss[i]
            cummSum[[i]] = cummSum[[i]] + cummSum[[i-1]]
        #}
    }
    
    totfreq = sum(freq_dist)
    mn = wgtedLss/ totfreq
    cummSum = cummSum / totfreq 

    testpercentile <- function(pc) {
        out = cummSum[[1]]
        out[mask] = 9E9
        for (i in rev(1:nlayers(cummSum)))
            out[cummSum[[i]] > pc] = i

        out[] = lss[out[]]
        return(out)
    }
    out95 = testpercentile(0.95) 
    out50 = testpercentile(0.50)  
    out = addLayer(mostFrequent, mn, out50, out95)

    bio = dnorm(lss, bioMean, bioSigma)
    bio = bio/sum(bio)
    
    out = addLayer(out, 
                   layer.apply(out[[1:3]], '-', bioMean),
                   out[[4]]  - lss[which(cumsum(bio) > 0.95)[1]])
   # dout[[1:3]] = dout[[1:3]]- bioMean
   # dout[[4]]   = out[[4]]  - lss[which(cumsum(bio) > 0.95)[1]]
    
    out = writeRaster(out, file = tfileAll, overwrite = TRUE)
    
    return(out)
}

enss = round(seq(1, nrow(ps), length.out = 100))
maxProb = model(which.max(ps[, 'lp__']), climMeanMap)
#uncProb = lapply(enss, model, climMeanMap)

outTab = rbind(ps[which.max(ps[, 'lp__']),], apply(ps, 2, quantile, c(0.05, 0.95)))
outTab = outTab[,c(-1, -ncol(outTab))]
outTab[,1] = exp(outTab[,1])*10000
########################
## plot maps          ##
########################
source("cfg.r")
climMean_limits = c(1, 2, 5, 10, 20, 40, 60, 80, 100)
climMean_cols = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')

plotMap <- function(r, cols, limits, addLeg = TRUE, units = 'c~m2~', extend_max = TRUE,...) {
    plotStandardMap(r, cols = cols, limits = limits, 
                    y_range = c(-60, 90), x_range = c(-180, 180)) 
    if (addLeg) {
        addStandardLegend(r, limits = limits, col = cols, srt = 0, oneSideLabels = FALSE,
                          extend_max = extend_max, units = units, ...)
    }
}
graphics.off()

png("figs/leafConstraintMaps.png", height = 9, width = 7.2, res = 300, units = 'in')
par(mfcol = c(5, 2), mar = rep(0, 4), oma = c(3, 0.5, 1, 0))
plotMap(exp(climMeanMap)*10000, limits = climMean_limits, cols = climMean_cols, addLeg = TRUE)
mtext(side = 2, 'Climate constraint', line = -0.5, adj = 0.35)
leafSizeLim = c(0.1, 0.2, 0.5, 1, 2, 4, 6, 8, 10, 15)
leafSize_cols = c('#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476',
                  '#41ab5d','#238b45','#006d2c','#00441b')
plotMaps <- function(rs, cols, limits, scale4 = 10, labs = c('', '', '', ''),
                     txtB = '', ...) {
    
    FUN <- function(r, txt, limitsi = limits,...) {
        plotMap(rs, limits = limitsi, cols = cols, ...)
        mtext(side = 2, txt, line = -0.5, adj = 0.35)
    }
    rs = exp(rs)
    FUN(rs[[1]], labs[1], addLeg = FALSE)
    FUN(rs[[2]], labs[2], addLeg = FALSE)
    FUN(rs[[3]], labs[3], ...)
    FUN(rs[[4]], labs[4], scale4*limits, ...)
    mtext(txtB, side = 1, line = 0.5)
    #layer.apply(rs[[1:2]], plotMap, 
    #            limits = limits, cols = cols, FALSE)
    #plotMap(rs[[3]], limits = limits, cols = cols, ...)
    #plotMap(rs[[4]], limits = scale4*limits, cols = cols, ...)
}

plotMaps(maxProb[[1:4]] + log(10000), leafSize_cols, leafSizeLim, 
         labs = c('Most frequent', 'Mean', '50% quantile', '95% quantile'), txtB = 'Leaf size')
plot.new()

dleafSizeLim = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
dleafSize_cols = c('#221100', '#FF9900', 'white')
plotMaps(maxProb[[5:8]], dleafSize_cols, limits = dleafSizeLim, 1, 
         extend_max = FALSE, maxLab = 1, units = '', txtB = 'Climate impact (ratio)')
dev.off()


##############################
## plot Sites               ##
##############################
xp = seq(-20, 20, 0.1)
labels = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500)
at = log(labels/10000)
plotGLobalSite <- function(id) {
    plot(c(-12, 1), c(0, 1), xlab = '', ylab = '', type = 'n', xaxt = 'n', yaxt= 'n')
    axis(1, at = at, labels = labels)

    obs =  log(leafSizes[which(sites == id)])
    climMean = log(climMeans[id])
    yclim = logistic(xp, climMean, -pss[1, 'climSigma'])
    
    polygon0 <- function(x, y,...) polygon(c(x[1], x, tail(x, 1)), c(0, y, 0), ...)
    
    polygon(c(-12, 1, 1, -12), c(0, 0, 1, 1), col = 'white', border = NA)
    polygon0(xp, ybio, border = NA, col = '#EE3300')
    lines(xp, yclim, col = 'blue', lwd = 2)
    lapply(obs, function(i) lines(c(i,i), c(0., 0.5)))
   
}


pss = ps[which.max(ps[, 'lp__']),]
ybio = dnorm(xp, pss[1, 'lsMu'], pss[1, 'lsSigma'])
ybio = ybio/max(ybio)
ncol = floor(sqrt(length(siteIDs)))
nrow = ceiling(length(siteIDs)/ncol)
png("figs/siteGlobalParams.png", height = nrow*1.5, width = ncol*1.5, units = 'in', res = 300)
par(mfrow = c(nrow, ncol), mar = rep(0.25, 4), oma = c(2, 2, 0, 0))
lapply(unique(sites), plotGLobalSite)
mtext.units (out = TRUE, side = 1, 'Leaf size (c~m2~)', line = 1.5)

dev.off()
