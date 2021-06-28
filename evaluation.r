source("cfg.r")
graphics.off()
summaryFile = "model_summary-nEns-11.nc"
PostDir = "data/sampled_posterior/attempt6/control/"
obsFile = 'data/driving_Data/TreeCover.nc'

limits = c(1, 2, 5, 10, 20,40, 60, 80)
cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679',
         '#41ab5d','#238443','#006837','#004529')

varname = "tree_cover_mean"

#########

sim = 100*brick.NaN(paste0(PostDir,summaryFile), varname = varname)[[c(2, 8)]]/0.8
obs = 100*raster(obsFile)/0.8
obs = raster::resample(obs, sim)(
if (F){
plotMap <- function(...) plotStandardMap(..., limits = limits, cols = cols)
png("figs/eval_mapComparison.png", height = 5.5, width = 7.2, units = 'in', res = 300) 
    par(mfrow = c(4, 1), mar = c(0.5, 0, 0.5, 0))
    plotMap(obs)
    mtext(side = 2, "VCF Observations", line = -1.5)
    plotMap(sim[[1]])
    mtext(side = 2, "Framework - 10%", line = -1.5)
    plotMap(sim[[2]])
    mtext(side = 2, "Framework - 90%", line = -1.5)
    addStandardLegend(obs, limits, cols, units = '%', srt = 0, add = FALSE,
                      plot_loc = c(0.2, 0.8, 0.73, 0.8), maxLab = 100)
dev.off()
}


####

dirs = list.dirs(PostDir)

obsT = 0.8*obs/100

bechmarkDir <- function(dir) {
    
    files = list.files(dir)
    nm = tail(strsplit(dir, '/')[[1]], 1)

    tfile = paste0("temp/MMcomp-", nm, "-", length(files), '.Rd')
    print(tfile)
    if (file.exists(tfile)) load(tfile) 
    else {
        Ps = read.csv(paste0(dir, '/',files[grepl('.csv', files)]))[,2]
        benchmarkFile <- function(file) {
            scores = score(NME(0.8*obs/100, w = raster::area(obs),
                           raster.NaN(paste0(dir, '/', file), varname = varname)))
            
            if (any(scores) > 1) browser()
            scores
        }
        files = files[grepl('.nc', files)]
        files = files[grepl('sample_no', files)]
        scores = sapply(files, benchmarkFile)
        save(scores, Ps, file = tfile)
    }
    return(list(scores, Ps))
}

scores = sapply(dirs[-1][1:6], bechmarkDir)

tfile = "temp/nullModel2.Rd"
if (file.exists(tfile)) {
    load(tfile)
} else {
    nulls = null.NME(obs, w= raster::area(obs), n = 1000)
    mdNull = nulls[[1]]
    mnNull = nulls[[2]]
    rrNull = nulls[[3]]
    save(mdNull, mnNull, rrNull, file = tfile) 
}

weightScores <- function(i) {
    out = unlist(lapply(1:length(scores[1,]),
                        function(j) scores[1,][[j]][i,] * scores[2,][[j]]))
    w = unlist(lapply(1:length(scores[1,]), function(j) scores[2,][[j]]))

    hist = hist(sample(out, 1000, replace = TRUE, prob = w), 100, plot = FALSE)

    browser()
#bind(unlist(out), unlist(w))
}
wscores = lapply(1:3, weightScores)
