###############
## set up    ##
###############
library(reldist)
library(fields)
library(rstan)

grab_cache = TRUE
filename = "data/leafsize_datasample.csv"
nchains = 2
nwarmup = 1000
niter = 10000
nrefresh = 250

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
run4Site <- function(siteID) {
    ofile = paste('outputs/rstan_contraints_for_site', siteID, nchains, nwarmup, niter, '.csv',
                  sep = '-')
    if (file.exists(ofile) && grab_cache) return(ofile)
    test = sites == siteID
    leafSize = log(leafSizes[test])
    climMean = log(climMeans[test][1])

    fit = stan(file = "leafSize/transform.stan",
               data = list(n = length(leafSize), LS = leafSize, cMu = climMean),
               chains = nchains,  warmup = nwarmup, iter = niter, cores = 1, refresh = 250,
               init = rep(list(list(lsMu = mean(leafSize), 
                                    lsSigma = sd(leafSize), climSd = 1)), nchains),
                control = list(max_treedepth = 10, adapt_delta = 0.95))
    params = data.frame( rstan::extract(fit))
    
    write.csv(params, file = ofile)
    return(ofile)
} 
               
out = lapply(unique(sites), run4Site)
    

