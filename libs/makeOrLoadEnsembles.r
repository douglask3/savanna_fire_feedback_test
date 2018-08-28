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
			sensitivity = loadInputData())
				
expNames = c('Control', 'MAP', 'MAT', 'Non-MAP climate', 'fire', 'Rainfall Distribution', 'temperature stress', 'population effect', 'urban area', 'cropland', 'pasture', 'humans', 'land use', 'sensitivity')

makeOrLoadEnsembles <- function(grab_cache = TRUE, invert = TRUE) {
	

	run_member <- function(line) {
		dname = paste(ens_dir, 'ensemble_', line, '/', sep = '')
		makeDir(dname)
		
		fnames =  paste(dname, names(dats), '.nc', sep = '')
		
		run <- function(dat, fname) {
			runCache <- function() {
				out = runLimTREE(line, dat, sensitivity = FALSE)
				if (grepl('sensitivity', fname)) {
					
					grad =  runLimTREE(line, dat, sensitivity = TRUE)
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
