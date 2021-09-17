source("cfg.r")
graphics.off()
options(scipen = 999)

cols = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
limits = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40)

index_file =  "index-nEns-11-from-101.csv"
PostDir = "data/sampled_posterior/attempt15/"

obsFile = 'data/driving_Data/TreeCover.nc'

biomes = round(raster::resample(raster('data/biomAssigned.nc'),
                          raster('data/driving_Data/TROPICS/TreeCover.nc')))

conID  = "control"
expIDs = c("Burnt\narea" = "noFire", "Heat\nStress" = "noTasMort","Wind" = "noWind",
            "Rainfall\ndistribution" = "noDrought", "Population\ndensity" = "noPop",
            "Urban\narea" = "noUrban", "Cropland\narea" = "noCrop", "Pasture\narea" = "noPas")
datFiles = paste0('data/driving_Data/TROPICS/',
                  c("BurntArea_", "MTWM.nc", "MaxWind.nc", "", "PopDen.nc", 
                    "urban.nc", "crop.nc", "pas.nc"))

bins = seq(0, 1, 0.01)
xs = bins[-1] - diff(bins)/2
openDats <- function(id, name, chist = NULL, ctr = NULL, axisT = FALSE) {
    openDat <- function(sample) {
        print(sample)
        dat = raster.NaN(paste0(PostDir, '/', id, '/', sample), varname = "tree_cover_mean")
        dat = dat/max.raster(dat, na.rm = TRUE)
        out = hist(dat, breaks = bins, plot = FALSE)$counts
    }
    tfile = paste0("temp/histgrammed-", id, "-", length(samples), ".Rd")
    if (file.exists(tfile)) load(tfile) else {
        out = sapply(samples, openDat)
        save(out, file = tfile)
    }
    out = apply(out, 1, quantile, c(0.1, 0.9))
    out = 1*out/max(out[,-(1:20)])
    plot(range(bins)*100, c(0, 1), type = 'n', xlim = c(20, 100), axes = FALSE)
    if (axisT) axis(1)
    mtext(side = 3, line = -1, adj = 0.1, name, padj = 1)
    polygonDist(xs*100, out[2,], col = "grey", border = NA)
    polygonDist(xs*100, out[1,], col = "black", border = NA)
    
    if (!is.null(chist)) {
        lines(xs*100, chist * out[1,50]/chist[50], col = 'white', lwd = 2)
        lines(xs*100, chist * out[1,50]/chist[50], lty = 2, lwd = 2)
    }
    if (!is.null(ctr))
        polygonDist(xs*100, ctr[1,], ctr[2,], col = make.transparent("red", 0.8), border = NA)
    return(out)
}

samples = list.files(paste0(PostDir, '/', conID), recursive = TRUE)
samples = samples[grepl('sample', samples)]
index = read.csv(paste0(PostDir, '/', conID, '/', index_file))
samples = samples[index[,2]]

mask = !is.na(raster.NaN(paste0(PostDir, conID, '/',samples[]), varname = "tree_cover_mean"))
obs = raster(obsFile)
obs = raster::resample(obs, mask)/0.8
obs[obs > 1] = 1
ctr = openDats(conID, 'Simulated')
dev.off()

png("figs/expHistogrammed.png", height = 12*6/5, width = 7.2, units = 'in', res = 300)

par(mfrow = c(5, 2), mar = rep(0.5, 4), oma = c(4, 0, 0, 0))

plot(range(bins)*100, c(0, 1), type = 'n', xlim = c(20, 100), axes = FALSE,
    xlab = '', ylab = '')

polyArea <- function(x)
    polygon(65 + x * 25 * c(-0.5, -0.5, 1.2, 1.2), c(0, 1, 1, 0),
            border = NA, col = make.transparent("black", 0.99))

lapply(seq(0, 1, by = 0.01), polyArea)
text(x = 70, y = 0.67, 'Tree cover\ngap', font = 2, cex = 1.3)

byBiome <- function(biome, r) {
    if (!is.na(biome)) r = r[biomes == biome]
    chist =  hist(r, breaks = bins, plot = FALSE)$counts
}
chist = sapply(c(NaN, 2:6, 8), byBiome, obs)
chist[,1] = chist[,2]
Mchist = max(chist[-(1:20), 1])
chist = chist/Mchist
chist = chist * ctr[1, 50] / chist[50, 1]

polyD <- function(i, col) polygonDist(xs*100, chist[,i], col = col, border = NA)

polyD(1, "black")

cols = c("Wet Forests" = "#7570b3", "Dry Forests" = "#d95f02", "Savanna" = "#66c2a5", 
          "Med." = "#8da0cb", "Summergreen" = "#1b9e77", 
            "Desert" = "#fc8d62")  
legend(x = 30, y = 1, names(cols)[-4], pch = 15, col = cols[-4], bty = 'n')
#browser()
for (i in 2:6) chist[,i] = apply(chist[,i:7], 1, sum)
mapply(polyD, 2:7, cols)

mtext(side = 3, adj = 0.9, line = -1, 'Observed', padj = 1)
chist = chist[,2]
ctr = openDats(conID, 'Simulated', chist = chist)
mapply(openDats, expIDs, names(expIDs), axisT = c(rep(F, 6), T, T), MoreArgs = list(ctr = ctr))

mtext(outer = TRUE, side = 1, "Tree cover (%)", line = 2)
dev.off()

hs = hist(logit(obs)[], 1000, plot = FALSE)[c('mids', 'density')]

hs[[2]] = log(hs[[2]]  + 0.00001)
png("figs/VCF_hist.png", height = 5, width = 5, res = 300, units = 'in')
plot(range(hs[[1]]), range(hs[[2]]), type = 'n', xlab = '', ylab = '', axes = FALSE)
polygonDist(hs[[1]], hs[[2]], rep(min(hs[[2]]), length(hs[[2]])), col = 'black')

mn = logit(mean.raster(obs, na.rm = TRUE))
sd = sqrt(sd(logit(obs)[], na.rm = TRUE))
x = seq(-12, 12, 0.001)
y = dnorm(x, mn, sd)
lines(x, log(y+0.00001), col = 'red', lwd = 3)

labels = c(0, 0.1, 1,  10, 50, 90, 99, 99.9, 100)
at = logit(labels/100)
axis(at = at, labels = labels, side = 1)

labels = c(0, 0.0001, 0.001, 0.01, 0.1, 1)*100
at = log(0.00001 + labels/100)
axis(2, labels = labels, at = at)

mtext(side = 1, 'Tree cover (%)', line = 2)
mtext(side = 2, 'Desnity (%)', line = 2)
dev.off()
