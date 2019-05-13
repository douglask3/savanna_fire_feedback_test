source("cfg.r")
graphics.off()

expIDS = c(2, 3, 14, 13, 5, 6, 7, 8, 9, 10, 11, 12)

TC_impact_tab <- function(pr_dataset, drought_var, ExpID = NULL, ...) {
    print(pr_dataset)
    print(drought_var)
    out = makeOrLoadEnsembles(pr_dataset = pr_dataset, drought_var = drought_var, ...)
    control = out[[1]]
    if (!is.null(ExpID)) Toff = out[ExpID] else Toff = out

    extractTC <- function(i) layer.apply(i, function(i) i[[1]])

    Toff = lapply(Toff, extractTC)
    control  = extractTC(control)
    
    diffArea <- function(r) {
        r = r * raster::area(r[[1]])
        out = layer.apply(r, sum.raster, na.rm = TRUE)
        return(unlist(out))
    }

    Tofft = lapply(Toff, diffArea)
    controlt = diffArea(control)

    pot = lapply(Tofft, function(i) 100*(i - controlt)/i)
    pot_mean = sapply(pot, mean)
    pot_sd = sapply(pot, sd)

    out = rbind(pot_mean, pot_sd)
    rownames(out) = paste(pr_dataset, drought_var, c('mean', 'sd'), sep = '_')
    return(out)
}

outs = runAll_pr_droughts(TC_impact_tab)

outs_out = c()
for (i in outs) for (j in i) {
    j = j[, expIDS]
    j[2, ] = j[2,]/j[1,]
    outs_out = cbind(outs_out, as.vector(j))
    colnames(outs_out) = c(head(colnames(outs_out), -1), 
                           strsplit(rownames(j)[1], '_mean'))
}

rownames(outs_out) = paste(rep(expNames[expIDS], each = 2), c('mean', 'sd'))
write.csv(outs_out, file = 'outputs/TCimpactTab.csv')
