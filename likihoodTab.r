source("cfg.r")
graphics.off()
options(scipen = 999)

cols = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
limits = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40)

index_file =  "index-nEns-1001-from-101.csv"
PostDir = "data/sampled_posterior/attempt15/"

conID  = "control"
expIDs = c("Burnt\narea" = "noFire", "Heat\nStress" = "noTasMort","Wind" = "noWind",
            "Rainfall\ndistribution" = "noDrought", "Population\ndensity" = "noPop",
            "Urban\narea" = "noUrban", "Cropland\narea" = "noCrop", "Pasture\narea" = "noPas")

samples = list.files(paste0(PostDir, '/', conID), recursive = TRUE)
samples = samples[grepl('sample', samples)]

index = read.csv(paste0(PostDir, '/', conID, '/', index_file))
samples = samples[index[,2]]

nexp = length(expIDs)
beats = matrix(0, nrow = nexp, ncol = nexp)
openCompareSample <- function(sample) {
    print(sample)
    tfile = paste0(paste0("temp/beats-", paste0(strsplit(sample, '/')[[1]], 
                                                collapse = '-')),
                   length(samples), '.Rd')
    if (file.exists(tfile)) {
        load(tfile)     
        return(beats)
    }
    print(which(samples==sample))
    openDat <- function(id) {
        dat = raster.NaN(paste0(PostDir, id, '/', sample), varname = "tree_cover_mean")
        sum.raster(dat, na.rm = TRUE)
    }
    dats = sapply(expIDs, openDat)
    for (i in 1:nexp) beats[i,] = beats[i,] + (dats[i] > dats)

    save(beats, file = tfile)
    return(beats)
}

beatss = lapply(rev(samples), openCompareSample)
for (i in beatss) beats = beats + i
beats = 100*beats/length(beatss)
rownames(beats) = expIDs
colnames(beats) = expIDs

write.csv(beats, file = "outputs/beatsTable.csv")


