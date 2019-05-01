########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

items = c(2:3, 5:6)

limits = list(c(0.01,0.05, 0.1, 0.2, 0.4, 0.6, 0.8),
              c(0.01,0.05, 0.1, 0.2, 0.4, 0.6, 0.8))

cols = list(c("#ffffd9", "#c7e9b4", "#41b6c4", "#225ea8", "#081d58"),
            c("#ffffe5", "#fee391", "#fe9929", "#cc4c02", "#662506"),
            c("#fff7f3", "#fcc5c0", "#f768a1", "#ae017e", "#49006a"),
            c("#fff7f3", "#222222"))

temp_file_controls = paste0('temp/', c('dat4control_plot', 'dat4conrolSen_plot'),'.Rd')

grab_controls <- function(pr_dataset, drought_var, sensitivity = FALSE) {
    out = makeOrLoadEnsembles(pr_dataset = pr_dataset, drought_var = drought_var)
    
    
    if (sensitivity) out = lapply(items, function(i)  tail(selectOutput(out, i), 1)[[1]])
        else out = lapply(items, function(i) 1-selectOutput(out, i)[[1]])

    out = lapply(out, function(i) addLayer(mean(i), sd.raster(i)))
    return(out)
}

plotControls <- function(sensitivity, temp_file, figName, lims) {
    if (!file.exists(temp_file)) {
        out = runAll_pr_droughts(grab_controls, sensitivity = sensitivity)
        save(out, file = temp_file)
    } else load(temp_file)
    
    figName = paste0('figs/', figName, '_all_data_maps.png')
    png(figName, height = 4.75*2* 1.15, width = 4.75*2, 
        units = 'in', res = 300)
        layout(t(matrix(c(1:72), nrow = 4)), heights = c(0.4, rep(1, 16), 0.4))
        par( mar = c(0, 0, 0, 0), oma = c(0, 3.7, 0, 0))
    
        addTitle <- function(txt) {
            plot.new()
            mtext(txt, side = 1, line = -1)
        }
        sapply(c("MAP", "MAT", "Stress", "Exclusions"), addTitle)

        plotMaps <- function(rs, let, dr, pr) {        
            txt = paste0(let,'\n', dr,'\n', pr)
            plotMap <- function(r, cols, txt, scaleBy) {
                plotStandardMap.sd(r[[1]] * scaleBy, limits = lims, cols = cols, e = r[[2]])
                mtext.units(side = 2, line = -0.5, adj = 0.1, txt)
            }
            mapply(plotMap,rs, cols, c(txt, '','',''),  c(1, 10, 1, 1))
        }
        plotPr <- function(dat, pr, lets)
            mapply(plotMaps, dat, lets, drought_vars, 
                   MoreArgs = list(pr))

        mapply(plotPr, out, pr_datasets, lapply(1:4, function(i) LETTERS[(1+(i-1)*4):(i*4)]))

        addLeg <- function(dat, limits_sc, cols) {
            plot.new()  
            addStandardLegend(dat[[1]], limits = lims * limits_sc, cols, units = '', 
                              maxLab = 1, plot_loc = c(0.02, 0.98, 0.77, 0.92)) 
        }
        mapply(addLeg, out[[1]][[1]], c(1, 0.10, 1,1), cols)
    dev.off()
    return(out)
}

outs = mapply(plotControls, c(FALSE, TRUE), temp_file_controls, 
             c('controls', 'senstivity'), limits)
