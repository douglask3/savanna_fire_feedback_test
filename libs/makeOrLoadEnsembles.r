dats = list(control   = loadInputData(),
				nofire    = loadInputData(remove = c("BurntArea")),
				noDrought = loadInputData(remove = c("Drought")),
				noPop     = loadInputData(remove = c("PopDen")),
				noCrop    = loadInputData(remove = c("crop")),
				noPas     = loadInputData(remove = c("pas")),
				noHumans  = loadInputData(remove = c("PopDen", "urban", "crop", "pas")))
				
expNames = c('Control', 'fire', 'drought', 'population effect', 'cropland', 'pasture', 'humans')

makeOrLoadEnsembles <- function(grab_cache = FALSE, invert =TRUE) {
	

	run_member <- function(line) {
		dname = paste(ens_dir, 'ensemble_', line, '/', sep = '')
		makeDir(dname)
		
		fnames =  paste(dname, names(dats), '.nc', sep = '')
		
		run <- function(dat, fname)
			out = runIfNoFile(fname, runLimTREE, line, dat, test = grab_cache)
		
		out = mapply(run, dats, fnames)
	}

	invertEnsemble <- function(ensembles)
		lapply(1:length(ensembles[[1]]), function(j) lapply(ensembles, function(i) i[[j]]))

	out = lapply(ensemble_no, run_member)
	if (invert) out = invertEnsemble(out)
	return(out)
}
