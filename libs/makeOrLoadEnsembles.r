dats = list(control     = loadInputData(),
			noMAP       = loadInputData(maxout = c("MAP")),
			noMAT       = loadInputData(replace = c("MAT" = 39)),
			noClimOther = loadInputData(replace = c("MAT" = 30), remove = c("MTWM", "Drought")),
			nofire      = loadInputData(remove = c("BurntArea")),
			noDrought   = loadInputData(remove = c("Drought")),
			noTempMort  = loadInputData(remove = c("MTWM")),
			noPop       = loadInputData(remove = c("PopDen")),
			noUrban     = loadInputData(remove = c("urban")),
			noCrop      = loadInputData(remove = c("crop")),
			noPas       = loadInputData(remove = c("pas")),
			noHumans    = loadInputData(remove = c("PopDen", "urban", "crop", "pas")),
			noExclusion = loadInputData(remove = c("urban", "crop", "pas")),
			noMortality = loadInputData(remove = c("MTWM", "PopDen", "Drought", "BurntArea")),
			sensitivity = loadInputData())
				
expNames = c('Control', 'MAP', 'MAT', 'Non-MAP climate', 'fire', 'Rainfall Distribution', 'temperature stress', 'population effect', 'urban area', 'cropland', 'pasture', 'humans', 'land use', 'sensitivity')

makeOrLoadEnsembles <- function(grab_cache = grab_cache_default, invert = TRUE,
                                pr_dataset = 'MSWEP', drought_var = 'MADD') {
	

	run_member <- function(line) {
		dname = paste0(ens_dir, pr_dataset, '_', drought_var, '/')
		makeDir(dname)	    
                dname = paste0(dname, 'sample_', line, '/')
		makeDir(dname)
    	
		fnames =  paste(dname, names(dats), '.nc', sep = '')
                paramFile = paste0(paramFile, '_', pr_dataset, '_', drought_var, '.csv')
		
		run <- function(dat, fname) {
                        pr_dat = dat[grepl(pr_dataset,names(dat))] 
                        dat['MAP']     = pr_dat[grepl('MAP_'     , names(pr_dat))]
                        dat['Drought'] = pr_dat[grepl(drought_var, names(pr_dat))]

			runCache <- function() {                                                                                out = runLimTREE(line, paramFile, dat, sensitivity = FALSE)
				if (grepl('sensitivity', fname)) {
					
					grad =  runLimTREE(line, paramFile, dat, sensitivity = TRUE)
					index = 1:(nlayers(grad) - 1)
					grad = layer.apply(index, function(i) grad[[-1]][[i]] * prod(out[[-1]][[-i]]))
					out = addLayer(out[[1]], grad)
				}
				return(out)
			}
			out = runIfNoFile(fname, runCache, test = grab_cache)
		}
		out = mapply(run, dats, fnames)
	}

	invertEnsemble <- function(ensembles)
		lapply(1:length(ensembles[[1]]), function(j) lapply(ensembles, function(i) i[[j]]))

	out = lapply(ensemble_no, run_member)
	if (invert) out = invertEnsemble(out)
	return(out)
}
