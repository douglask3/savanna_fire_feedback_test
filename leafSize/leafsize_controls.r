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

## for for sites with more than this number of samples
nsiteMin = 50

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
siteIDs = siteIDs[test]#[1:2]

out = lapply(siteIDs, run4Site)

nplots = length(siteIDs)
nrow = floor(sqrt(nplots))
ncol = ceiling(nplots/nrow)

par(mfrow = c(nrow, ncol), mar = c(1.5, 0.1, 0.1, 0.1))

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

    xrange = range(c(x, log10(exp(climMean)+2)))
    plot(x, y, type = 'h', axes = FALSE, xlim = xrange, ylim = c(0, 1))
    axis(1, at = (-(20):20), 10^(-(20):20))

    xmod = seq(-20, 20, 0.1)
    xp = log10(exp(xmod))
    addMod <- function(p) {
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

mapply(plotSite, siteIDs, out)

