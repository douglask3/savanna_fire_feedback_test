dats_info = list(control     = list(NULL),
                 noMAP       = list(maxout = c("MAP")),
                 noMAT       = list(replace = c("MAT" = 39)),
                 noClimOther = list(replace = c("MAT" = 30), 
                                    remove = c("MTWM", "Drought")),
                 nofire      = list(remove = c("BurntArea")),
                 noDroughtMAP= list(remove = c("RainTerm_Drought")),
                 noDroughtSTS= list(remove = c("StressTerm_Drought")),
                 noDrought   = list(remove = c("RainTerm_Drought",
                                               "StressTerm_Drought")),
                 noHeatStres = list(remove = c("MTWM")),
                 noColdStres = list(replace = c("MTCM" = 1000)),
                 noPop       = list(remove = c("PopDen")),
                 noUrban     = list(remove = c("urban")),
                 noCrop      = list(remove = c("crop")),
                 noPas       = list(remove = c('buffalo', 'cattle', 'goat', 'sheep')),
                 noHumans    = list(remove = c("PopDen", "urban", "crop",
                                               'buffalo', 'cattle', 'goat', 'sheep')),
                 noExclusion = list(remove = c("urban", "crop",
                                                'buffalo', 'cattle', 'goat', 'sheep')),
                 noMortality = list(remove = c("MTWM", "PopDen", "Drought", "BurntArea")),
                 sensitivity = list())

load_dats <- function(andFire = FALSE) {
    loadDat <- function(args) {
        if (andFire) args[["remove"]] = c(args[["remove"]], "BurntArea")
        do.call(loadInputData, args)
    }
    if (andFire) {
        if (!exists("dats_fire"))
            dats_fire <<- lapply(dats_info, loadDat)
            
        dats_out = dats_fire
    } else {
        if (!exists("dats")) 
            dats <<- lapply(dats_info, loadDat)            
        
        dats_out = dats
    }
    return(dats_out)
}
			
			
expNames = c('Control', 'MAP', 'MAT', 'Non-MAP climate', 'Burnt area',
             'Rainfall dist. on MAP', 'Rainfall dist. on Stress', 'Rainfall distribution', 
             'Heat stress', 'Cold stress', 'Population density', 
             'Urban area', 'Cropland', 'Pasture', 'All human impacts',
             'Land use', 'No Mort', 'Sensitivity')

makeOrLoadEnsembles <- function(grab_cache = grab_cache_default, invert = TRUE,
                                fire_dataset = "GFED_four_s",
                                pr_dataset = 'MSWEP', drought_var = 'MADD',
                                andFire = FALSE) {
    datsi = lapply(andFire, load_dats)
    
    run_member <- function(dati, andFirei, line) {
        dname = paste0(ens_dir, fire_dataset, '_', pr_dataset, '_', drought_var, '/')
        makeDir(dname)	    
        dname = paste0(dname, 'sample_', line, '/')
        makeDir(dname)
        
        if (andFirei) dname = paste0(dname, 'Fire', '/')
        else dname = paste0(dname, 'noFire', '/')
        makeDir(dname)
            
        fnames =  paste(dname, names(dati), '.nc', sep = '')
        paramFile = paste0(paramFile, '_',
                           fire_dataset, '_', pr_dataset, '_', drought_var, '.csv')
            
        run <- function(dat, fname) {
            pr_dat = dat[grepl(pr_dataset,names(dat))] 
            dat['MAP']     = pr_dat[grepl('MAP_'     , names(pr_dat))]
                
            dat['RainTerm_Drought'] =  pr_dat[grepl(paste0("RainTerm_", drought_var),names(pr_dat))]
            dat['StressTerm_Drought'] = pr_dat[grepl(paste0("StressTerm_", drought_var), names(pr_dat))]

            runCache <- function() {                                                                            out = runLimTREE(line, paramFile, dat, sensitivity = FALSE)
                if (grepl('sensitivity', fname)) {					
                    grad =  runLimTREE(line, paramFile, dat, sensitivity = TRUE)
                    index = 1:(nlayers(grad) - 1)
                    grad = layer.apply(index, function(i) grad[[-1]][[i]] * 
                                                              prod(out[[-1]][[-i]]))
                    out = addLayer(out[[1]], grad)
                }
                return(out)
            }
            
            out = runIfNoFile(fname, runCache, test = grab_cache)
        }
        out = mapply(run, dati, fnames)
    }
    
    runDati <- function(...) {
        out = mapply(run_member, datsi, andFire, MoreArgs = list(...))
        
        outCache <- function(r) {
            fname = filename(r[[1]][[1]])
            fname = strsplit(fname, 'Fire')[[1]]
            dir = paste0(fname[1], 'DiffFire')
            makeDir(dir)
            fname = paste0(dir, fname[2])
            
            out = runIfNoFile(fname, function() r[[2]] - r[[1]], test = grab_cache)
            
            return(out)
        }
        if (ncol(out) == 2) out = apply(out, 1, outCache)
        return(out)
    }        
    out = lapply(ensemble_no, runDati)
    
    invertEnsemble <- function(ensembles)
        lapply(1:length(ensembles[[1]]), function(j) lapply(ensembles, function(i) i[[j]]))

   
    if (invert) out = invertEnsemble(out)
    return(out)
}
