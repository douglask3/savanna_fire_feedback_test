###############
## set up    ##
###############
library(reldist)
library(fields)
library(rstan)
source("libs/return_multiple_from_functions.r")

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
#dat =  data_global#evergreen_dat#read.csv(filename)
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

runBayesian.prescribedClim <- function(leafSize, climMean) {
  
  runBayesian("leafSize/prescribedClim.stan", 
              list(nl = length(leafSize), ns =length(climMean), 
                   LS = leafSize, siteIDs = sites, cMu = climMean, #
                   LSmean = mean(leafSize), LSsd = sd(leafSize), climsd = 1),
              list(lsMu = mean(leafSize), lsSigma = sd(leafSize), climSigma = 1))
}

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
  #  dnorm(xmod, p['lsMu'], p['lsSigma'] * logistic(x, x0 
  
  fit = FUN(leafSize, climMean)
  
  params = data.frame(rstan::extract(fit))
  browser()
  write.csv(params, file = ofile)
  return(ofile)
} 

#siteIDs = unique(sites)
#test = sapply(siteIDs, function(siteID) sum(sites == siteID) > nsiteMin)
#siteIDs = siteIDs[test]
#
outGlobal = run4Site(NaN)
browser()
#outPrescribed = lapply(siteIDs, run4Site)

plotSite <- function(siteID, pfile) {
  rm(test,climImpact,climMean)
  
  ps = read.csv(pfile)
  ps = ps[sample(1:nrow(ps), 1000),]
  # browser()
  c(leafSize, climMean) := selectDat(siteID)
  leafSize = log10(exp(leafSize))
  ls = hist(leafSize, length(leafSize)/10, plot = FALSE)
  
  y = ls[['density']]/max(ls[['density']])
  x = ls[['mids']]
  
  xrange = range(c(x, log10(exp(climMean)+2)))
  plot(x, y, type = 'h', axes = T, xlim = xrange, ylim = c(0, 1))
  
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
  
  test <- cbind(climImpact,ps)
  test[which(test$climImpact == max(test$climImpact)),'climSigma']
  
  climImpact = round(quantile(100*climImpact, c(.05, 0.95)), 2)
  
  
  mtext(side = 1, line = -7, adj = 0.1, paste0(climImpact[1], '-', climImpact[2], '%'),col='blue')    
  
  addHistLine <- function(x, y, ...) 
    mapply(function(i, j, ...) lines(c(i, i), c(0, j), ...), x, y)
  
  addHistLine(x, y)
  
  lsMuRng = quantile(log10(exp(ps[['lsMu']])), c(0.1, 0.9))
  points(lsMuRng, y = c(-0.0, -0.0), xpd = TRUE, pch = 19)
  lines(lsMuRng, c(0, 0))
  
  lssigma = quantile(ps[['lsMu']], c(0.1,0.5, 0.9))
  
  write.csv(cbind(test[which(test$climImpact == max(test$climImpact)),],siteID,climMean,lssigma[[1]], lssigma[[2]],lssigma[[3]]), file = paste('temp/max_for_site',  siteID, 'Prescribed_param.csv', sep = '-'))
  
  return(climImpact)
  
}
