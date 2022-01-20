###############
## set up    ##
###############
library(reldist)
library(fields)
library(rstan)
source("libs/return_multiple_from_functions.r")

grab_cache = TRUE
filename = "data/leafsize_datasample.csv"
nchains = 3
nwarmup = 250
niter = 2000
nrefresh = 250

## for for sites with more than this number of samples
nsiteMin = 200

###############
## open      ##
###############
dat = read.csv(filename)
leafSizes = dat[['LS']]/10000
climMeans = 10^dat[['pmin']]
sites = dat[['site.f']]

###############
## run       ##
###############

selectDat <- function(siteID) {
    test = sites == siteID
    leafSize = log(leafSizes[test])
    climMean = log(climMeans[test][1])
    return(list(leafSize, climMean))
}

runBayesian <- function(file, data, init, ...)  {
    #data[[2]] = data[[2]]/(1+exp(0.3*(data[[2]]+5)))
    #browser()
    fit = stan(file = file, data = data,
               chains = nchains,  warmup = nwarmup, iter = niter, cores = 1, refresh = 250,
               init = rep(list(init), nchains),
               control = list(max_treedepth = 10, adapt_delta = 0.95))
}
runBayesian.prescribedClim <- function(leafSize, climMean) 
    runBayesian("leafSize/prescribedClim.stan", 
                list(n = length(leafSize), LS = leafSize, cMu = climMean),
                list(lsMu = mean(leafSize), lsSigma = sd(leafSize), climSd = 1))


runBayesian.varyingClim <- function(leafSize, climMean) 
    runBayesian("leafSize/varyingClim.stan", 
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

               
siteIDs = unique(sites)
test = sapply(siteIDs, function(siteID) sum(sites == siteID) > nsiteMin)
siteIDs = siteIDs[test][1:2]

outPrescribed = lapply(siteIDs, run4Site)
outVarying = lapply(siteIDs, run4Site, runBayesian.varyingClim, "varying")

nplots = length(siteIDs)
nrow = floor(sqrt(nplots))
ncol = ceiling(nplots/nrow)

par(mfrow = c(nrow, ncol), mar = c(1.5, 0.1, 0.1, 0.1))

logistic <- function(x, x0 = 0, k = 1) 
    1/(1 + exp(-k*(x - x0)))

plotSite <- function(siteID, pfile) {
    ps = read.csv(pfile)
    ps = ps[sample(1:nrow(ps), 1000),]
    browser()
    c(leafSize, climMean) := selectDat(siteID)
    leafSize = log10(exp(leafSize))
    ls = hist(leafSize, length(leafSize)/10, plot = FALSE)
    
    y = ls[['density']]/max(ls[['density']])
    x = ls[['mids']]

    xrange = range(c(x, log10(exp(climMean)+2)))
    plot(x, y, type = 'h', axes = FALSE, xlim = xrange, ylim = c(0, 1))
    axis(1, at = (-(20):20), 10^(-(20):20))

    xmod = seq(-20, 20, 0.1)
    xp = log10(exp(xmod))
    addMod <- function(p) {
        #browser()
        if (!is.na(p['cMu'])) {
            lines(c(climMean, climMean), c(0, 1), col = 'blue', lwd = 2, lty = 2)
            climMean = p['cMu']
        }
        yclim = logistic(xmod, climMean, -p['climSigma'])
        lines(xp, yclim, col = '#0000FF01', lwd = 2)
        
        ybio = dnorm(xmod, p['lsMu'], p['lsSigma'])
        ybio = 0.67*ybio/max(ybio)
        if (sample(c(T,F), 1, prob = c(0.1, 0.9)))
            polygon(xp, ybio, border = NA, col = '#FF000005')   

        1-sqrt(sum(ybio*ybio*yclim))/sqrt(sum(ybio*ybio))
    }
    climImpact = apply(ps, 1, addMod)
    climImpact = round(quantile(100*climImpact, c(.05, 0.95)), 2)
    mtext(side = 1, line = -5, adj = 0.1, paste0(climImpact[1], '-', climImpact[2], '%'))    
    addHistLine <- function(x, y, ...) 
        mapply(function(i, j, ...) lines(c(i, i), c(0, j), ...), x, y)

    addHistLine(x, y)
    lsMuRng = quantile(log10(exp(ps[['lsMu']])), c(0.1, 0.9))
    points(lsMuRng, y = c(-0.0, -0.0), xpd = TRUE, pch = 19)
    lines(lsMuRng, c(0, 0))
    return(climImpact)
    
}
mapply(plotSite, siteIDs, outVarying)
#mapply(plotSite, siteIDs, outPrescribed)


