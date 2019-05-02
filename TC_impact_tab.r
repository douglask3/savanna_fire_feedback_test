source("cfg.r")
graphics.off()


TC_impact_tab <- function(pr_dataset, drought_var, ExpID = 5:12, ...) {
    print(pr_dataset)
    print(drought_var)
    out = makeOrLoadEnsembles(pr_dataset = pr_dataset, drought_var = drought_var, ...)
    control = out[[1]]
    Toff = out[ExpID]

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
for (i in outs) for (j in i) outs_out = rbind(outs_out, j)

colnames(outs_out) = expNames[5:12]
write.csv(outs_out, file = 'outputs/TCimpactTab.csv')
