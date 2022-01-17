###############
## set up    ##
###############
library(reldist)
library(fields)
library(rstan)
source("libs/return_multiple_from_functions.r")

grab_cache = TRUE
filename = "data/leafsize_datasample.csv"
nchains = 10
nwarmup = 250
niter = 2000
nrefresh = 250

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

run4Site <- function(siteID) {
    ofile = paste('outputs/rstan_contraints_for_site', siteID, nchains, nwarmup, niter, '.csv',
                  sep = '-')
    if (file.exists(ofile) && grab_cache) return(ofile)
       
    c(leafSize, climMean) := selectDat(siteID)
    fit = stan(file = "leafSize/transform.stan",
               data = list(n = length(leafSize), LS = leafSize, cMu = climMean),
               chains = nchains,  warmup = nwarmup, iter = niter, cores = 1, refresh = 250,
               init = rep(list(list(lsMu = mean(leafSize), 
                                    lsSigma = sd(leafSize), climSd = 1)), nchains),
                control = list(max_treedepth = 10, adapt_delta = 0.95))
    params = data.frame(rstan::extract(fit))
    
    write.csv(params, file = ofile)
    return(ofile)
} 
               
siteIDs = unique(sites)
test = sapply(siteIDs, function(siteID) sum(sites == siteID) > nsiteMin)
siteIDs = siteIDs[test][1:2]
browser()
out = lapply(siteIDs, run4Site)

nplots = length(siteIDs)
nrow = floor(sqrt(nplots))
ncol = ceiling(nplots/nrow)

#par(mfrow = c(nrow, ncol), mar = rep(0.1, 4))

logistic <- function(x, x0 = 0, k = 1) 
    1/(1 + exp(-k*(x - x0)))

plotSite <- function(siteID, pfile) {
    ps = read.csv(pfile)
    ps = ps[sample(1:nrow(ps), 1000),]
    c(leafSize, climMean) := selectDat(siteID)
    leafSize = log10(exp(leafSize))
    ls = hist(leafSize, length(leafSize)/10, plot = FALSE)
    
    y = ls[['density']]/max(ls[['density']])
    x = ls[['mids']]

    xrange = range(c(x, log10(exp(climMean))))
    plot(x, y, type = 'h', axes = FALSE, xlim = xrange)
    axis(1, at = (-(20):20), 10^(-(20):20))
    #browser()

    xp = seq(xrange[1], xrange[2], length.out = 100)
    addMod <- function(p) {
        y = logistic(log(10^xp), climMean, -p['climSigma'])
        lines(xp, y, col = '#0000FF01', lwd = 2)
        if (min(y) > 0.9) browser()
        y = dnorm(log(10^xp), p['lsMu'], p['lsSigma'])
        polygon(xp, y, border = NA, col = '#FF000001')
        #browser()
    }
    apply(ps, 1, addMod)
    lsMuRng = quantile(log10(exp(ps[['lsMu']])), c(0.1, 0.9))
    points(lsMuRng, y = c(-0.0, -0.0), xpd = TRUE, pch = 19)
    lines(lsMuRng, c(0, 0))
    
    plot(x, y, type = 'h', add = TRUE)
    
}

mapply(plotSite, siteIDs, out)

